'vWATCH64 - A debug/variable watch system for QB64 programs
'Fellippe Heitor, 2015-2020 - fellippeheitor@gmail.com - @fellippeheitor
'
'Code repository: https://github.com/FellippeHeitor/vWATCH64
'
'Requirements: QB64 1.3.
'------------------------------------------------------------------------------

$VERSIONINFO:FILEVERSION#=1,2,0,0
$VERSIONINFO:PRODUCTVERSION#=1,2,0,0
$VERSIONINFO:CompanyName=Fellippe Heitor
$VERSIONINFO:FileDescription=vWATCH64 - A debug/variable watch system for QB64 programs
$VERSIONINFO:FileVersion=v1.2
$VERSIONINFO:InternalName=vwatch.bas
$VERSIONINFO:LegalCopyright=Open source
$VERSIONINFO:OriginalFilename=vwatch.exe
$VERSIONINFO:ProductName=vWATCH64
$VERSIONINFO:ProductVersion=v1.2
$VERSIONINFO:Comments=Requires the latest build of QB64
$VERSIONINFO:Web=www.qb64.org/vwatch
'------------------------------------------------------------------------------

$EXEICON:'./resources/wasp.ico'
_ICON

$RESIZE:ON
DEFLNG A-Z

$IF WIN THEN
    DECLARE LIBRARY
        FUNCTION PlaySound (pszSound AS STRING, BYVAL hmod AS INTEGER, BYVAL fdwSound AS INTEGER)
    END DECLARE

    DECLARE DYNAMIC LIBRARY "kernel32"
        FUNCTION OpenProcess& (BYVAL dwDesiredAccess AS LONG, BYVAL bInheritHandle AS LONG, BYVAL dwProcessId AS LONG)
        FUNCTION CloseHandle& (BYVAL hObject AS LONG)
        FUNCTION GetExitCodeProcess& (BYVAL hProcess AS LONG, lpExitCode AS LONG)
    END DECLARE
$ELSE
    DECLARE LIBRARY
    FUNCTION PROCESS_CLOSED& ALIAS kill (BYVAL pid AS INTEGER, BYVAL signal AS INTEGER)
    END DECLARE
$END IF

DECLARE CUSTOMTYPE LIBRARY
    SUB memcpy (BYVAL dest AS _OFFSET, BYVAL source AS _OFFSET, BYVAL bytes AS LONG)
END DECLARE

'Constants: -------------------------------------------------------------------
CONST ID = "vWATCH64"
CONST VERSION = "1.2.0"

CONST LF = 10
CONST TIMEOUTLIMIT = 10 'SECONDS

'Messagebox
CONST MB_CUSTOM = -1
CONST OK_ONLY = 0
CONST YN_QUESTION = 1
CONST MB_YES = 6
CONST MB_NO = 7

'OpenInclude return codes:
CONST MERGESUCCESSFUL = 0
CONST MISSINGFILE = -1
CONST NOINCLUDES = -2

'Breakpoint control:
CONST CONTINUE = 1
CONST NEXTSTEP = 2
CONST READY = 3
CONST SETVAR = 4
CONST SKIPSUB = 5
CONST SETNEXT = 7

'Filters:
CONST VARIABLENAMES = 1
CONST VALUES = 2
CONST SCOPE = 3
CONST DATATYPES = 4
CONST CODE = 5
CONST LINENUMBERS = 6
'CONST SETNEXT = 7

'Screen:
CONST DEFAULT_WIDTH = 1000
CONST DEFAULT_HEIGHT = 600
CONST SCREEN_TOPBAR = 50

'Custom data types: -----------------------------------------------------------
TYPE HEADERTYPE
    CLIENT_ID AS STRING * 8
    VERSION AS STRING * 5
    CONNECTED AS _BYTE
    RESPONSE AS _BYTE
    PID AS LONG
END TYPE

TYPE CLIENTTYPE
    NAME AS STRING * 256
    CHECKSUM AS STRING * 8
    TOTALSOURCELINES AS LONG
    EXENAME AS STRING * 256
    LINENUMBER AS LONG
    TOTALVARIABLES AS LONG
    PID AS LONG
END TYPE

TYPE BREAKPOINTTYPE
    ACTION AS _BYTE
    LINENUMBER AS LONG
END TYPE

TYPE VARIABLESTYPE
    NAME AS STRING * 256
    SCOPE AS STRING * 50
    UDT AS STRING * 40
    DATATYPE AS STRING * 20
END TYPE

TYPE VARIABLEVALUETYPE
    VALUE AS STRING * 256
END TYPE

TYPE WATCHPOINTTYPE
    EXPRESSION AS STRING * 256
END TYPE

TYPE UDTTYPE
    UDT AS STRING * 40
    ELEMENT AS STRING * 256
    DATATYPE AS STRING * 20
END TYPE

TYPE SUBFUNC_TYPE
    NAME AS STRING * 50
    LINE AS LONG
    ENDING AS LONG
    EXTERNAL AS _BYTE
END TYPE

TYPE BUTTONSTYPE
    ID AS LONG
    CAPTION AS STRING * 120
    X AS INTEGER
    Y AS INTEGER
    W AS INTEGER
END TYPE

'Shared variables: ------------------------------------------------------------
DIM SHARED BREAKPOINTLIST AS STRING
DIM SHARED CLIENT_CURRENTMODULE AS STRING * 50
DIM SHARED POSITION_IN_LINE AS LONG
DIM SHARED DEFAULTDATATYPE(65 TO 90) AS STRING * 20
DIM SHARED EXCHANGEDATASIZE$4
DIM SHARED EXCHANGEDATA AS STRING
DIM SHARED FILE AS INTEGER
DIM SHARED FILENAME$
DIM SHARED FILEERRORRAISED AS _BYTE
DIM SHARED FIRSTEXECUTION AS _BYTE
DIM SHARED CONVERSIONERRORRAISED AS _BYTE
DIM SHARED PAGE_HEIGHT AS LONG
DIM SHARED INTERNALKEYWORDS AS INTEGER
DIM SHARED GETELEMENT_LASTPOSITION AS LONG
DIM SHARED LIST_AREA AS INTEGER
DIM SHARED LINE_TRAIL AS INTEGER
DIM SHARED LONGESTLINE AS LONG
DIM SHARED LONGESTSCOPESPEC AS LONG
DIM SHARED LONGESTVARNAME AS LONG
DIM SHARED MAINSCREEN AS LONG
DIM SHARED MENU%
DIM SHARED MESSAGEBOX_RESULT AS INTEGER
DIM SHARED NEWFILENAME$
DIM SHARED RUNTOTHISLINE AS LONG
DIM SHARED SB_TRACK AS INTEGER
DIM SHARED SELECTED_VARIABLES AS STRING
DIM SHARED SCREEN_WIDTH AS INTEGER
DIM SHARED SCREEN_HEIGHT AS INTEGER
DIM SHARED SET_OPTIONBASE AS INTEGER
DIM SHARED SOURCEFILE AS STRING
DIM SHARED CHECKINGOFF_LINES AS STRING
DIM SHARED TITLESTRING AS STRING
DIM SHARED TOTALBREAKPOINTS AS LONG
DIM SHARED TOTALSKIPLINES AS LONG
DIM SHARED TOTALVARIABLES AS LONG
DIM SHARED TOTAL_SELECTEDVARIABLES AS LONG
DIM SHARED TOTALWATCHPOINTS AS LONG
DIM SHARED WATCHPOINTLIST AS STRING
DIM SHARED WATCHPOINTBREAK AS LONG
DIM SHARED PATHSEP$

'File structure:
DIM SHARED HEADERBLOCK AS LONG
DIM SHARED CLIENTBLOCK AS LONG
DIM SHARED BREAKPOINTBLOCK AS LONG
DIM SHARED BREAKPOINTLISTBLOCK AS LONG
DIM SHARED DATAINFOBLOCK AS LONG
DIM SHARED DATABLOCK AS LONG
DIM SHARED WATCHPOINTLISTBLOCK AS LONG
DIM SHARED WATCHPOINTEXPBLOCK AS LONG
DIM SHARED WATCHPOINTCOMMANDBLOCK AS LONG
DIM SHARED EXCHANGEBLOCK AS LONG

'Custom structures:
DIM SHARED BREAKPOINT AS BREAKPOINTTYPE
DIM SHARED WATCHPOINT_COMMAND AS BREAKPOINTTYPE
DIM SHARED CLIENT AS CLIENTTYPE
DIM SHARED HEADER AS HEADERTYPE

'Eye-candy control
DIM SHARED SetPause#, SetRun#, ShowPauseIcon, ShowRunIcon
DIM SHARED WASP AS LONG

'Switches:
DIM SHARED DONTCOMPILE AS _BYTE
DIM SHARED STEPMODE AS _BYTE
DIM SHARED SKIPARRAYS AS _BYTE
DIM SHARED USERQUIT AS _BYTE
DIM SHARED CLOSE_SESSION AS _BYTE
DIM SHARED DEBUGGEE_CLOSED AS _BYTE
DIM SHARED TRACE AS _BYTE
DIM SHARED VARIABLE_HIGHLIGHT AS _BYTE

'Dynamic arrays:
REDIM SHARED QB64KEYWORDS(0) AS STRING
REDIM SHARED SOURCECODE(0) AS STRING
REDIM SHARED SOURCECODE_COLORIZED(0) AS _BYTE
REDIM SHARED SUBFUNC(0) AS SUBFUNC_TYPE
REDIM SHARED VARIABLES(0) AS VARIABLESTYPE
REDIM SHARED VARIABLE_DATA(0) AS VARIABLEVALUETYPE
REDIM SHARED WATCHPOINT(0) AS WATCHPOINTTYPE

DIM i AS INTEGER

'Variables initialization: ----------------------------------------------------
SET_DEF "A-Z", "SINGLE"
SET_OPTIONBASE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0
VARIABLE_HIGHLIGHT = -1
SCREEN_WIDTH = DEFAULT_WIDTH
SCREEN_HEIGHT = DEFAULT_HEIGHT
LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR

READ_KEYWORDS

'Screen setup: ----------------------------------------------------------------
WASP = WASP_IMAGE&
MAINSCREEN = _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
SCREEN MAINSCREEN
DO: _LIMIT 30: LOOP UNTIL _SCREENEXISTS
TITLESTRING = "vWATCH64 - v" + VERSION
_TITLE TITLESTRING

'QB64 version check: ----------------------------------------------------------
IF (INSTR(_OS$, "WIN") > 0 AND _FILEEXISTS("qb64.exe") = 0) OR (INSTR(_OS$, "WIN") = 0 AND _FILEEXISTS("qb64") = 0) THEN
    MESSAGEBOX_RESULT = MESSAGEBOX(ID, "vWATCH64 must be in QB64's main folder.", MKI$(OK_ONLY), 1, 0)
    SYSTEM
ELSE
    IF _FILEEXISTS("source/qb64.bas") THEN
        OPEN "source/qb64.bas" FOR BINARY AS #1
        a$ = SPACE$(LOF(1))
        GET #1, , a$
        CLOSE #1
        IF INSTR(a$, "IF FileDropEnabled = 0 THEN FileDropEnabled = -1: _ACCEPTFILEDROP") = 0 THEN 'Snippet added to QB64 v1.3
            MESSAGEBOX_RESULT = MESSAGEBOX(ID, "vWATCH64 needs a newer version of QB64.", MKI$(OK_ONLY), 1, 0)
            SYSTEM
        END IF
        a$ = ""
    ELSE
        MESSAGEBOX_RESULT = MESSAGEBOX(ID, "vWATCH64 needs a newer version of QB64.", MKI$(OK_ONLY), 1, 0)
        SYSTEM
    END IF
END IF

'Restore 'timers.h' -----------------------------------------------------------
RESTORE_LIBRARY

'Parse the command line: ------------------------------------------------------
'Did the user drag a .BAS file onto this program or enter parameters?
'Syntax: VWATCH64 [source filename.bas] [-options] [-target <newfilename>]
'If no parameters are passed, vWATCH64 starts in MONITOR MODE
IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT > 1 THEN
        'Set flags based on command line arguments:
        FOR i = 1 TO _COMMANDCOUNT
            SELECT CASE LCASE$(COMMAND$(i))
                CASE "--dontcompile", "-d": DONTCOMPILE = -1
                CASE "--noarrays", "-n": SKIPARRAYS = -1
                CASE "--target", "-t": IF i < _COMMANDCOUNT THEN NEWFILENAME$ = COMMAND$(i + 1): i = i + 1
                CASE ELSE
                    'Any other arguments are ignored.
            END SELECT
        NEXT i
    END IF
END IF

$IF WIN THEN
    PATHSEP$ = "\"
$ELSE
    PATHSEP$ = "/"
$END IF

IF LEN(COMMAND$) THEN
    IF _FILEEXISTS(COMMAND$(1)) THEN FILENAME$ = COMMAND$(1): PROCESSFILE ELSE MESSAGEBOX_RESULT = MESSAGEBOX(ID, "File not found.", MKI$(OK_ONLY), 1, 0)
    NEWFILENAME$ = ""
END IF

GOTO MainLoop

OpenFileMenu:
IF SCREEN_WIDTH < DEFAULT_WIDTH OR SCREEN_HEIGHT < DEFAULT_HEIGHT THEN CHECK_RESIZE DEFAULT_WIDTH, DEFAULT_HEIGHT
_RESIZE OFF
PCOPY 0, 1
MESSAGEBOX_RESULT = INPUTBOX("Open and Process .BAS", "File to open:", "", FILENAME$, 0, 0)
_RESIZE ON

'Reset flags:
SET_DEF "A-Z", "SINGLE"
SET_OPTIONBASE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0

tryOpen:
IF LEN(FILENAME$) THEN
    IF _FILEEXISTS(FILENAME$) THEN
        PROCESSFILE
    ELSE
        IF RIGHT$(LCASE$(FILENAME$), 4) <> ".bas" THEN FILENAME$ = FILENAME$ + ".bas": GOTO tryOpen
        MESSAGEBOX_RESULT = MESSAGEBOX(ID, "File not found.", MKI$(OK_ONLY), 1, 0)
    END IF
END IF
NEWFILENAME$ = ""

'------------------------------------------------------------------------------
MainLoop:
'------------------------------------------------------------------------------
DO
    _RESIZE OFF
    TITLESTRING = "vWATCH64 - v" + VERSION
    _TITLE TITLESTRING
    SETUP_CONNECTION
    IF MENU% = 101 THEN GOTO OpenFileMenu
    IF _TOTALDROPPEDFILES > 0 THEN FILENAME$ = _DROPPEDFILE$(1): _FINISHDROP: GOTO tryOpen
    _RESIZE ON
    SOURCE_VIEW
LOOP UNTIL USERQUIT
'------------------------------------------------------------------------------
SYSTEM

FileError:
FILEERRORRAISED = -1
RESUME NEXT

DataConversionERROR:
CONVERSIONERRORRAISED = -1
RESUME NEXT

DataTypeKeywordDATA:
DATA _BIT,_UNSIGNED _BIT,_BYTE,_UNSIGNED _BYTE,INTEGER
DATA _UNSIGNED INTEGER,LONG,_UNSIGNED LONG,_INTEGER64
DATA _UNSIGNED _INTEGER64,SINGLE,DOUBLE,_FLOAT,STRING
DATA **END**

QB64KeywordsDATA:
DATA $IF,$ELSE,$END
DATA _ALPHA,_ALPHA32,_AUTODISPLAY,_AXIS,_BACKGROUNDCOLOR,_BIT,
DATA _BLEND,_BLUE,_BLUE32,_BUTTON,_BUTTONCHANGE,_BYTE,$CHECKING
DATA _CLEARCOLOR,_CLIP,_CLIPBOARD$,_CONNECTED,_CONNECTIONADDRESS$
DATA $CONSOLE,_CONSOLE,_CONSOLETITLE,_CONTROLCHR,_COPYIMAGE,_COPYPALETTE
DATA _CV,_DEFAULTCOLOR,_DEFINE,_DELAY,_DEST,_DEST,_DEVICE$,_DEVICEINPUT
DATA _DEVICES,_DIREXISTS,_DISPLAY,_DISPLAY,_DONTBLEND,_DONTWAIT
DATA _ERRORLINE,_EXIT,_FILEEXISTS,_FLOAT,_FONT,_FONT,_FONTHEIGHT
DATA _FONTWIDTH,_FREEFONT,_FREEIMAGE,_FREETIMER,_FULLSCREEN,_FULLSCREEN
DATA _GREEN,_GREEN32,_HEIGHT,_HIDE,_ICON,_INTEGER64,_KEYHIT,_KEYDOWN
DATA _LASTAXIS,_LASTBUTTON,_LASTWHEEL,_LIMIT,_LOADFONT,_LOADIMAGE
DATA _MAPTRIANGLE,_MAPUNICODE,_MEM,_MEMCOPY,_MEMELEMENT,_MEMFILL
DATA _MEMFREE,_MEMGET,_MEMIMAGE,_MEMNEW,_MEMPUT,_MIDDLE,_MK$,_MOUSEBUTTON
DATA _MOUSEHIDE,_MOUSEINPUT,_MOUSEMOVE,_MOUSEMOVEMENTX,_MOUSEMOVEMENTY
DATA _MOUSESHOW,_MOUSEWHEEL,_MOUSEX,_MOUSEY,_NEWIMAGE,_NONE,_OFFSET
DATA _OPENCLIENT,_OPENCONNECTION,_OPENHOST,_OS$,_PALETTECOLOR,_PIXELSIZE
DATA _PRESERVE,_PRINTIMAGE,_PRINTMODE,_PRINTSTRING,_PRINTWIDTH,_PUTIMAGE
DATA _RED,_RED32,$RESIZE,_RESIZE,_RGB,_RGB32,_RGBA,_RGBA32
DATA _ROUND,_SCREENCLICK,$SCREENHIDE,_SCREENHIDE,_SCREENIMAGE,_SCREENMOVE
DATA _SCREENPRINT,$SCREENSHOW,_SCREENSHOW,_SCREENX,_SCREENY,_SETALPHA
DATA _SHELLHIDE,_SNDBAL,_SNDCLOSE,_SNDCOPY,_SNDGETPOS,_SNDLEN,_SNDLIMIT
DATA _SNDLOOP,_SNDOPEN,_SNDPAUSE,_SNDPAUSED,_SNDPLAY,_SNDPLAYCOPY
DATA _SNDPLAYFILE,_SNDPLAYING,_SNDRATE,_SNDRAW,_SNDRAWLEN,_SNDRAWDONE
DATA _SNDRAWOPEN,_SNDSETPOS,_SNDSTOP,_SNDVOL,_SOURCE,_TITLE,_UNSIGNED
DATA _WHEEL,_WIDTH,ABS,ABSOLUTE,ACCESS,ALIAS,AND,ANY,APPEND
DATA AS,ASC,ATN,BEEP,BINARY,BLOAD,BSAVE,BYVAL,CALL,CALLS
DATA CASE,CDBL,CDECL,CHAIN,CHDIR,CHR$,CINT,CIRCLE,CLEAR,CLNG
DATA CLOSE,CLS,COLOR,COMMAND$,COMMON,CONST,COS,CSNG,CSRLIN
DATA CVD,CVDMBF,CVI,CVL,CVS,CVSMBF,DATA,DATE$,DECLARE,DEF
DATA DEFDBL,DEFINT,DEFLNG,DEFSNG,DEFSTR,DIM,DO,DOUBLE,DRAW
DATA $DYNAMIC,ELSE,ELSEIF,END,ENVIRON,EOF,EQV,ERASE,ERDEV
DATA ERL,ERR,ERROR,EXIT,EXP,FIELD,FILEATTR,FILES,FIX,FOR
DATA FRE,FREE,FREEFILE,FUNCTION,GET,GOSUB,GOTO,HEX$,IF,IMP
DATA $INCLUDE,INKEY$,INP,INPUT,INSTR,INT,INTEGER,INTERRUPT
DATA INTERRUPTX,IOCTL,IOCTL$,IS,KEY,KILL,LBOUND,LCASE$,LEFT$,LEN
DATA LET,LINE,LIST,LOC,LOCATE,LOCK,LOF,LOG,LONG,LOOP,LPOS
DATA LPRINT,LSET,LTRIM$,MID,MID$,MKD$,MKDIR,MKDMBF$,MKI$,MKL$
DATA MKS$,MKSMBF$,MOD,NAME,NEXT,NOT,OCT$,OFF,ON,OPEN,OPTION
DATA OR,OUT,OUTPUT,PAINT,PALETTE,PCOPY,PEEK,PEN,PLAY,PMAP
DATA POINT,POKE,POS,PRESET,PRINT,PSET,PUT,RANDOM,RANDOMIZE
DATA READ,REDIM,REM,RESET,RESTORE,RESUME,RETURN,RIGHT$,RMDIR
DATA RND,RSET,RTRIM$,RUN,SADD,SCREEN,SEEK,SEG,SELECT,SETMEM,SGN
DATA SHARED,SHELL,SIGNAL,SIN,SINGLE,SLEEP,SOUND,SPACE$,SPC
DATA SQR,STATIC,$STATIC,STEP,STICK,STOP,STR$,STRIG,STRING
DATA STRING$,SUB,SWAP,SYSTEM,TAB,TAN,THEN,TIME$,TIMER,TO
DATA TROFF,TRON,TYPE,UBOUND,UCASE$,UEVENT,UNLOCK,UNTIL,VAL
DATA VARPTR,VARPTR$,VARSEG,VIEW,WAIT,WEND,WHILE,WIDTH,WINDOW
DATA WRITE,XOR,_CEIL,BASE,_EXPLICIT,_INCLERRORLINE,_DIR$
DATA _INCLERRORFILE$,$EXEICON,**END**

'------------------------------------------------------------------------------
'SUBs and FUNCTIONs:                                                          -
'------------------------------------------------------------------------------
SUB SOURCE_VIEW
    'Allows setting breakpoints and stepping through code
    DIM SB_Ratio AS SINGLE
    DIM SourceLine AS STRING
    DIM ListEnd_Label AS STRING

    STATIC SearchIn
    STATIC PrevSearchIn, PrevFilter$
    STATIC TempQuickWatchVars AS LONG

    TotalButtons = 7
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    TOTALSKIPLINES = 0
    BREAKPOINT.ACTION = 0 'Start paused; execution starts with F5 or F8.
    BREAKPOINT.LINENUMBER = 0
    PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS , _RGB32(255, 255, 255)

    Filter$ = ""
    IF SearchIn = 0 THEN SearchIn = CODE
    SB_ThumbY = 0
    grabbedY = -1
    SB_X = 1
    ListEnd_Label = "(end of source file)"
    ShowTempMessage = 0
    STEPMODE = -1
    TRACE = -1
    _KEYCLEAR
    CLOSE_SESSION = 0
    DEBUGGEE_CLOSED = 0
    FIRSTEXECUTION = -1

    DO: _LIMIT 500
        GOSUB ProcessInput
        GET #FILE, CLIENTBLOCK, CLIENT
        FIND_CURRENTMODULE
        PUT #FILE, BREAKPOINTLISTBLOCK, BREAKPOINTLIST
        PUT #FILE, WATCHPOINTLISTBLOCK, WATCHPOINTLIST
        PUT #FILE, WATCHPOINTEXPBLOCK, WATCHPOINT()
        GET #FILE, DATABLOCK, VARIABLE_DATA()
        GET #FILE, WATCHPOINTCOMMANDBLOCK, WATCHPOINT_COMMAND

        IF LEN(SOURCEFILE) > 0 THEN
            FOR i = 1 TO CLIENT.TOTALVARIABLES
                IF (TRIM$(UCASE$(VARIABLES(i).SCOPE)) <> UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2))) AND TRIM$(VARIABLES(i).SCOPE) <> "SHARED" THEN
                    VARIABLE_DATA(i).VALUE = "<out of scope>"
                END IF
            NEXT i
        END IF

        IF CLIENT.LINENUMBER <> prevLineNumber THEN
            prevLineNumber = CLIENT.LINENUMBER
            IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
                STEPMODE = -1
                SetPause# = TIMER
                ShowPauseIcon = -1
                ShowRunIcon = 0
                IF CLIENT.LINENUMBER = RUNTOTHISLINE THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    RUNTOTHISLINE = 0
                    FOR MultiLineToggle = CLIENT.LINENUMBER + 1 TO CLIENT.TOTALSOURCELINES
                        IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                            ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, CLIENT.LINENUMBER)
                        ELSE
                            EXIT FOR
                        END IF
                    NEXT MultiLineToggle
                END IF
            END IF

            IF WATCHPOINT_COMMAND.ACTION = NEXTSTEP THEN
                WATCHPOINTBREAK = WATCHPOINT_COMMAND.LINENUMBER
                STEPMODE = -1
                SetPause# = TIMER
                ShowPauseIcon = -1
                ShowRunIcon = 0
                WATCHPOINT_COMMAND.ACTION = READY
                WATCHPOINT_COMMAND.LINENUMBER = 0
                PUT #FILE, WATCHPOINTCOMMANDBLOCK, WATCHPOINT_COMMAND
                GOSUB ClearTempQuickWatchVars
                VARIABLE_VIEW
                GOSUB AddTempQuickWatchVar
            END IF

            'Dynamically add QUICK WATCH variables based on the current line of code
            GOSUB AddTempQuickWatchVar
        END IF

        GOSUB UpdateList

        IF _EXIT THEN USERQUIT = -1
        SEND_PING
    LOOP UNTIL HEADER.CONNECTED = 0 OR USERQUIT OR CLOSE_SESSION OR DEBUGGEE_CLOSED

    EndMessage:
    IF CLOSE_SESSION OR USERQUIT THEN
        HEADER.CONNECTED = 0
        PUT #FILE, HEADERBLOCK, HEADER
        CLOSE #FILE
        ON ERROR GOTO FileError
        DO WHILE _FILEEXISTS(_CWD$ + PATHSEP$ + "vwatch64.dat")
            _LIMIT 10
            KILL _CWD$ + PATHSEP$ + "vwatch64.dat"
        LOOP
        ON ERROR GOTO 0
        EXIT SUB
    END IF

    IF USERQUIT THEN EXIT SUB

    IF HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED THEN
        EndMessage$ = "Connection closed by client."
    END IF

    IF HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED THEN
        MESSAGEBOX_RESULT = MESSAGEBOX(ID, EndMessage$, MKI$(OK_ONLY), 1, -1)
    END IF
    EXIT SUB

    ProcessInput:
    k = _KEYHIT: modKey = k
    IF modKey = 100303 OR modKey = 100304 THEN shiftDown = -1
    IF modKey = -100303 OR modKey = -100304 THEN shiftDown = 0
    IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
    IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0

    DO
        prevy = y
        y = y + (_MOUSEWHEEL * ((_HEIGHT - 50) / 5))
        IF y <> prevy THEN TRACE = 0
        mx = _MOUSEX
        my = _MOUSEY
        mb = _MOUSEBUTTON(1)
        mb2 = _MOUSEBUTTON(2)
    LOOP WHILE _MOUSEINPUT

    SELECT EVERYCASE k
        CASE 32 TO 126 'Printable ASCII characters
            SELECT CASE SearchIn
                CASE CODE
                    Filter$ = Filter$ + CHR$(k)
                CASE LINENUMBERS
                    IF (k >= 48 AND k <= 57) OR (k = 45) OR (k = 44) THEN
                        Filter$ = Filter$ + CHR$(k)
                    ELSE
                        Filter$ = Filter$ + CHR$(k)
                        SearchIn = CODE
                    END IF
                CASE SETNEXT
                    IF (k >= 48 AND k <= 57) THEN Filter$ = Filter$ + CHR$(k)
            END SELECT
        CASE 8 'Backspace
            IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
        CASE 9 'TAB
            IF SearchIn = SETNEXT THEN
                SearchIn = CODE
                Filter$ = ""
            ELSEIF SearchIn = CODE THEN
                SearchIn = LINENUMBERS
            ELSE
                SearchIn = CODE
            END IF

            IF LEN(Filter$) > 0 AND VAL(Filter$) = 0 AND SearchIn = LINENUMBERS THEN Filter$ = ""
        CASE 13 'ENTER confirms SET NEXT LINE, after proper evaluation of Filter$
            IF SearchIn = SETNEXT THEN
                IF LEN(Filter$) = 0 THEN
                    SearchIn = PrevSearchIn
                    Filter$ = PrevFilter$
                ELSEIF LEN(Filter$) > 0 AND VAL(Filter$) > 0 THEN
                    DesiredLine = VAL(Filter$)
                    SetNext_Click:
                    IF DesiredLine <= CLIENT.TOTALSOURCELINES THEN
                        DesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(DesiredLine)))
                        IF LEN(DesiredSourceLine$) > 0 THEN
                            IF ASC(DesiredSourceLine$, 1) = 1 OR ASC(DesiredSourceLine$, 1) = 3 THEN DesiredSourceLine$ = MID$(DesiredSourceLine$, 2)
                        END IF
                        PrevDesiredSourceLine$ = " "
                        IF DesiredLine > 1 THEN PrevDesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(DesiredLine - 1)))
                        CanGo = -1
                        IF LEN(DesiredSourceLine$) = 0 THEN CanGo = 3
                        IF RIGHT$(PrevDesiredSourceLine$, 1) = "_" THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 1) = "$" THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 4) = "DIM " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 5) = "DATA " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 5) = "CASE " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 5) = "TYPE " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 6) = "REDIM " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 6) = "CONST " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "STATIC " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "DEFINT " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "DEFLNG " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "DEFSTR " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "DEFSNG " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 7) = "DEFDBL " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 8) = "DECLARE " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 8) = "_DEFINE " THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 11) = "END DECLARE" THEN CanGo = 3
                        IF LEFT$(DesiredSourceLine$, 16) = "OPTION _EXPLICIT" THEN CanGo = 3
                        IF CanGo = -1 THEN
                            IF TRIM$(CLIENT_CURRENTMODULE) = "MAIN MODULE" THEN
                                CanGo = -1
                                DeclaringLibrary = 0
                                FOR dl.Check = DesiredLine TO 1 STEP -1
                                    SearchedLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(dl.Check)))
                                    IF LEN(SearchedLine$) > 0 THEN
                                        IF ASC(SearchedLine$, 1) = 1 OR ASC(SearchedLine$, 1) = 3 THEN SearchedLine$ = MID$(SearchedLine$, 2)
                                    END IF
                                    IF GETELEMENT$(SearchedLine$, 1) = "DECLARE" AND FIND_SYMBOL(1, SearchedLine$, "LIBRARY") THEN
                                        DeclaringLibrary = 0
                                    ELSEIF GETELEMENT$(SearchedLine$, 1) = "END" AND GETELEMENT$(SearchedLine$, 2) = "DECLARE" THEN
                                        DeclaringLibrary = -1
                                    END IF
                                    IF (GETELEMENT$(SearchedLine$, 1) = "SUB" OR GETELEMENT$(SearchedLine$, 1) = "FUNCTION") AND DeclaringLibrary = 0 THEN
                                        CanGo = 1
                                        EXIT FOR
                                    END IF
                                NEXT dl.Check
                            ELSE
                                CanGo = 2
                                FOR dl.Check = DesiredLine TO 1 STEP -1
                                    SearchedLine$ = TRIM$(GETLINE$(dl.Check))
                                    IF LEN(SearchedLine$) > 0 THEN
                                        IF ASC(SearchedLine$, 1) = 1 OR ASC(SearchedLine$, 1) = 3 THEN SearchedLine$ = MID$(SearchedLine$, 2)
                                    END IF
                                    cm$ = CLIENT_CURRENTMODULE
                                    IF (GETELEMENT$(SearchedLine$, 1) = "SUB" OR GETELEMENT$(SearchedLine$, 1) = "FUNCTION") THEN
                                        IF GETELEMENT$(SearchedLine$, 2) = GETELEMENT$(cm$, 2) THEN
                                            'We're in the same module as the desired line
                                            CanGo = -1
                                            EXIT FOR
                                        ELSE
                                            EXIT FOR
                                        END IF
                                    END IF
                                NEXT dl.Check
                            END IF
                        END IF
                        IF CanGo = -1 THEN
                            BREAKPOINT.ACTION = SETNEXT
                            BREAKPOINT.LINENUMBER = DesiredLine
                            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
                            BREAKPOINT.ACTION = 0
                            BREAKPOINT.LINENUMBER = 0
                            IF NOT Clicked THEN
                                SearchIn = PrevSearchIn
                                Filter$ = PrevFilter$
                            END IF
                        ELSEIF CanGo = 1 OR CanGo = 2 THEN
                            Message$ = ""
                            Message$ = Message$ + "Next line must be " + IIFSTR$(CanGo = 1, "in the main module", "inside " + GETELEMENT$(cm$, 1) + " " + GETELEMENT$(cm$, 2)) + CHR$(LF)
                            MESSAGEBOX_RESULT = MESSAGEBOX("Outside boundaries", Message$, MKI$(OK_ONLY), 1, -1)
                        ELSE
                            Message$ = ""
                            Message$ = Message$ + "The specified source line can't be set as the next statement" + CHR$(LF)
                            MESSAGEBOX_RESULT = MESSAGEBOX("Nonexecutable statement", Message$, MKI$(OK_ONLY), 1, -1)
                        END IF
                    ELSE
                        Message$ = "Invalid line number."
                        MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MKI$(OK_ONLY), 1, -1)
                    END IF
                    IF Clicked THEN Clicked = 0: RETURN
                END IF
            END IF
        CASE 27 'ESC clears the current search filter or exits interactive mode
            ExitButton_Click:
            IF (SearchIn = CODE OR SearchIn = LINENUMBERS) AND LEN(Filter$) > 0 THEN
                Filter$ = ""
            ELSEIF SearchIn = SETNEXT THEN
                SearchIn = PrevSearchIn
                Filter$ = PrevFilter$
            ELSE
                CLOSE_SESSION = -1
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 18432 'Up
            ArrowStep! = ((_HEIGHT - 50) * SB_Ratio)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - ArrowStep!
            TRACE = 0
        CASE 20480 'Down
            ArrowStep! = ((_HEIGHT - 50) * SB_Ratio)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + ArrowStep!
            TRACE = 0
        CASE 19200 'Left
            IF SB_X > 1 THEN SB_X = SB_X - 1
        CASE 19712 'Right
            IF SB_X < LONGESTLINE THEN SB_X = SB_X + 1
        CASE 16128 'F5
            RunButton_Click:
            FIRSTEXECUTION = 0
            IF WATCHPOINTBREAK > 0 THEN
                IF ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 1 THEN
                    Message$ = "Execution was halted on a watchpoint (" + TRIM$(VARIABLES(WATCHPOINTBREAK).NAME) + TRIM$(WATCHPOINT(WATCHPOINTBREAK).EXPRESSION) + ")" + CHR$(LF)
                    Message$ = Message$ + "Clear it before resuming?"
                    MESSAGEBOX_RESULT = MESSAGEBOX("Run/Resume", Message$, MKI$(YN_QUESTION), 1, -1)
                    IF MESSAGEBOX_RESULT = MB_YES THEN
                        ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 0
                        TOTALWATCHPOINTS = TOTALWATCHPOINTS - 1
                        WATCHPOINT(WATCHPOINTBREAK).EXPRESSION = ""
                    END IF
                END IF
            END IF
            IF STEPMODE = -1 THEN SetRun# = TIMER: ShowRunIcon = -1: ShowPauseIcon = 0
            STEPMODE = 0
            WATCHPOINTBREAK = 0
            BREAKPOINT.ACTION = CONTINUE
            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16384 'F6
            WindowButton_Click:
            IF CLIENT.TOTALVARIABLES > 0 THEN
                _KEYCLEAR
                GOSUB ClearTempQuickWatchVars
                VARIABLE_VIEW
                GOSUB AddTempQuickWatchVar
            ELSE
                Message$ = "There are no variables in your watch list."
                MESSAGEBOX_RESULT = MESSAGEBOX("No variables", Message$, MKI$(OK_ONLY), 1, -1)
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16896 'F8
            StepButton_Click:
            FIRSTEXECUTION = 0
            IF shiftDown THEN
                IF SOURCECODE_COLORIZED(CLIENT.LINENUMBER) = 0 THEN ADDCOLORCODE CLIENT.LINENUMBER
                IF INSTR(SOURCECODE(CLIENT.LINENUMBER), CHR$(5)) > 0 THEN
                    StepOverMenu_Click:
                    IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                    STEPMODE = -1
                    TRACE = -1
                    BREAKPOINT.ACTION = SKIPSUB
                    PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
                    IF Clicked THEN Clicked = 0: RETURN
                ELSE
                    GOTO NormalF8
                END IF
            ELSE
                NormalF8:
                IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                STEPMODE = -1
                TRACE = -1
                BREAKPOINT.ACTION = NEXTSTEP
                PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 17152 'F9
            ToggleButton_Click:
            IF LEN(FilteredList$) = 0 THEN
                IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 AND GETELEMENT$(GETLINE$(CLIENT.LINENUMBER), 1) <> "STOP" THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                ELSEIF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 AND GETELEMENT$(GETLINE$(CLIENT.LINENUMBER), 1) = "STOP" THEN
                    Message$ = ""
                    Message$ = Message$ + "The STOP statement creates a permanent breakpoint." + CHR$(LF)
                    Message$ = Message$ + "Would you like to skip this line instead?"
                    IF MESSAGEBOX("Permanent breakpoint", Message$, MKI$(YN_QUESTION), 1, -1) = MB_YES THEN
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                        TOTALSKIPLINES = TOTALSKIPLINES + 1
                        ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 2
                    END IF
                ELSE
                    IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 2 THEN TOTALSKIPLINES = TOTALSKIPLINES - 1
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                END IF
                FOR MultiLineToggle = CLIENT.LINENUMBER + 1 TO CLIENT.TOTALSOURCELINES
                    IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                        ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, CLIENT.LINENUMBER)
                    ELSE
                        EXIT FOR
                    END IF
                NEXT MultiLineToggle
            ELSE
                FOR setAll = 1 TO LEN(FilteredList$) / 4
                    which_Line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_Line) = 0 AND LEN(STRIPCOMMENTS$(GETLINE$(which_Line))) THEN
                        IF ASC(BREAKPOINTLIST, which_Line) = 2 THEN TOTALSKIPLINES = TOTALSKIPLINES - 1
                        ASC(BREAKPOINTLIST, which_Line) = 1
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                        FOR MultiLineToggle = which_Line + 1 TO CLIENT.TOTALSOURCELINES
                            IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, which_Line)
                            ELSE
                                EXIT FOR
                            END IF
                        NEXT MultiLineToggle
                    END IF
                NEXT setAll
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 17408 'F10
            ClearButton_Click:
            IF LEN(FilteredList$) = 0 THEN
                FOR clear.BP = 1 TO CLIENT.TOTALSOURCELINES
                    IF ASC(BREAKPOINTLIST, clear.BP) = 1 AND GETELEMENT$(GETLINE$(clear.BP), 1) <> "STOP" THEN ASC(BREAKPOINTLIST, clear.BP) = 0: TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                NEXT clear.BP
            ELSE
                FOR setAll = 1 TO LEN(FilteredList$) / 4
                    which_Line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_Line) = 1 AND GETELEMENT$(GETLINE$(which_Line), 1) <> "STOP" THEN
                        ASC(BREAKPOINTLIST, which_Line) = 0
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                        FOR MultiLineToggle = which_Line + 1 TO CLIENT.TOTALSOURCELINES
                            IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, which_Line)
                            ELSE
                                EXIT FOR
                            END IF
                        NEXT MultiLineToggle
                    END IF
                NEXT setAll
            END IF
            IF Clicked THEN Clicked = 0: RETURN
    END SELECT

    'Scrollbar check:
    IF PAGE_HEIGHT > LIST_AREA THEN
        IF mb THEN
            IF mx > _WIDTH(MAINSCREEN) - 30 AND mx < _WIDTH(MAINSCREEN) THEN
                TRACE = 0
                'Clicked inside the scroll bar. Check if click was on the thumb:
                IF my > SCREEN_TOPBAR + SB_ThumbY + 24 AND my < SCREEN_TOPBAR + SB_ThumbY + 24 + SB_ThumbH THEN
                    'Clicked on the thumb:
                    grabbedY = my: starty = y
                    DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
                    DO WHILE _MOUSEBUTTON(1): _LIMIT 500
                        WHILE _MOUSEINPUT: WEND
                        my = _MOUSEY
                        y = starty + ((my - grabbedY) / SB_Ratio)

                        CHECK_SCREEN_LIMITS y
                        IF prevy <> y THEN
                            DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my: prevy = y
                            GOSUB UpdateList
                        END IF
                        SEND_PING
                        _DISPLAY
                    LOOP
                    grabbedY = -1
                ELSEIF my > SCREEN_TOPBAR AND my <= SCREEN_TOPBAR + 20 THEN
                    'Up arrow
                    IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - (LIST_AREA / 10)
                    _DELAY .1
                ELSEIF my > SCREEN_HEIGHT - 21 THEN
                    'Down arrow
                    IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + (LIST_AREA / 10)
                    _DELAY .1
                ELSE
                    'Clicked above or below the thumb:
                    IF my < SCREEN_TOPBAR + 25 + SB_ThumbY AND my > SCREEN_TOPBAR + 21 THEN
                        y = y - ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    ELSEIF my > SCREEN_TOPBAR + 25 + SB_ThumbY + SB_ThumbH AND my < SCREEN_HEIGHT - 21 THEN
                        y = y + ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    END IF
                END IF
            END IF
        END IF
    END IF
    RETURN

    UpdateList:
    CHECK_RESIZE 0, 0
    LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR - ((TOTAL_SELECTEDVARIABLES + 1) * _FONTHEIGHT)
    CLS , _RGB32(255, 255, 255)
    cursorBlink% = cursorBlink% + 1
    IF cursorBlink% > 50 THEN cursorBlink% = 0

    IF LEN(SOURCEFILE) > 0 THEN
        'Build a filtered list, if a filter is active:
        i = 0: FilteredList$ = ""
        PAGE_HEIGHT = _FONTHEIGHT * (CLIENT.TOTALSOURCELINES + 3)
        IF LEN(Filter$) > 0 AND SearchIn <> SETNEXT THEN
            DO
                i = i + 1
                IF i > CLIENT.TOTALSOURCELINES THEN EXIT DO
                Found = 0
                IF SearchIn = CODE THEN Found = MULTI_SEARCH(UCASE$(GETLINE$(i)), UCASE$(Filter$))
                IF SearchIn = LINENUMBERS THEN Found = INTERVAL_SEARCH(Filter$, i)
                IF Found THEN
                    FilteredList$ = FilteredList$ + MKL$(i)
                END IF
            LOOP
            IF LEN(FilteredList$) > 0 THEN PAGE_HEIGHT = _FONTHEIGHT * ((LEN(FilteredList$) / 4) + 3)
        END IF

        'Scroll to the next line of code that will be run
        IF TRACE AND LEN(FilteredList$) = 0 THEN
            CurrentLineY = (CLIENT.LINENUMBER - 1) * _FONTHEIGHT
            IF CurrentLineY > y + LIST_AREA - _FONTHEIGHT THEN
                y = (CurrentLineY - LIST_AREA) + SCREEN_TOPBAR
            ELSEIF CurrentLineY < y THEN
                y = CurrentLineY - SCREEN_TOPBAR + (_FONTHEIGHT * 3)
            END IF
        END IF

        CHECK_SCREEN_LIMITS y

        CLS , _RGB32(255, 255, 255)
        'Print list items to the screen:
        IF LEN(FilteredList$) > 0 THEN
            ListStart = ((y \ _FONTHEIGHT) + 1)
            ListEnd = LEN(FilteredList$) / 4
            FOR ii = ListStart TO ListEnd
                i = CVL(MID$(FilteredList$, ii * 4 - 3, 4))
                SourceLine = GETLINE$(i)
                printY = (SCREEN_TOPBAR + 3 + ((ii - 1) * _FONTHEIGHT)) - y
                IF printY > SCREEN_HEIGHT THEN EXIT FOR
                IF (printY >= (SCREEN_TOPBAR - _FONTHEIGHT)) AND printY < SCREEN_HEIGHT THEN
                    'Print only inside the program area
                    GOSUB ColorizeList
                    IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
                    IF MenuWasInvoked THEN MenuWasInvoked = 0: RETURN
                    IF SOURCECODE_COLORIZED(i) = 0 THEN ADDCOLORCODE i
                    v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), IIFSTR$(ASC(BREAKPOINTLIST, i) = 2, CHR$(9), " ")) + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
                    PRINT_COLORIZED 5 - ((SB_X - 1) * _FONTWIDTH), printY, v$, i
                    COLOR _RGB32(0, 0, 0)
                END IF
            NEXT ii
        ELSEIF LEN(Filter$) = 0 OR SearchIn = SETNEXT THEN
            ListStart = ((y \ _FONTHEIGHT) + 1)
            ListEnd = CLIENT.TOTALSOURCELINES
            FOR i = ListStart TO ListEnd
                SourceLine = GETLINE$(i)
                printY = (SCREEN_TOPBAR + 3 + ((i - 1) * _FONTHEIGHT)) - y
                IF printY > SCREEN_HEIGHT THEN EXIT FOR
                'Print only inside the program area
                GOSUB ColorizeList
                IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
                IF MenuWasInvoked THEN MenuWasInvoked = 0: RETURN
                IF SOURCECODE_COLORIZED(i) = 0 THEN ADDCOLORCODE i
                v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), IIFSTR$(ASC(BREAKPOINTLIST, i) = 2, CHR$(9), " ")) + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
                PRINT_COLORIZED 5 - ((SB_X - 1) * _FONTWIDTH), printY, v$, i
                COLOR _RGB32(0, 0, 0)
            NEXT i
        END IF

        IF LEN(Filter$) AND LEN(FilteredList$) = 0 AND SearchIn <> SETNEXT THEN 'A filter is on, but nothing was found
            _PRINTSTRING (5, 4 * _FONTHEIGHT), "Search terms not found."
            _PRINTSTRING (5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to reset filter)"
        END IF

        IF PAGE_HEIGHT > LIST_AREA THEN
            IF LEN(FilteredList$) > 0 THEN
                _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
            ELSE
                _PRINTSTRING (5, ((5 + CLIENT.TOTALSOURCELINES) * _FONTHEIGHT) - y), ListEnd_Label
            END IF
            DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
        ELSE
            'End of list message:
            IF LEN(FilteredList$) > 0 THEN
                _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
            ELSE
                _PRINTSTRING (5, PAGE_HEIGHT + (_FONTHEIGHT * 2) - y), ListEnd_Label
            END IF
        END IF
    ELSE
        LINE (0, SCREEN_TOPBAR)-STEP(_WIDTH, _HEIGHT - SCREEN_TOPBAR), _RGB32(200, 200, 200), BF
        Message$ = "<Source file(s) changed/not found>"
        COLOR _RGB32(255, 0, 0)
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(Message$) / 2, SCREEN_TOPBAR + (_HEIGHT - SCREEN_TOPBAR) / 2 - _FONTHEIGHT / 2), Message$
    END IF

    'Top bar:
    '  SOURCE VIEW: <F5 = Run> <Trace ???> <F6 = View Variables> <F8 = Step> <F9 = Toggle Breakpoint> <ESC = Exit>
    '  Breakpoints 0 * Next line: ####
    '  Filter (code):
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), SCREEN_TOPBAR), _RGB32(179, 255, 255), BF
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    ModeTitle$ = "SOURCE VIEW: "
    _PRINTSTRING (5, 3), ModeTitle$
    COLOR _RGB32(255, 255, 255)
    _PRINTSTRING (4, 2), ModeTitle$
    COLOR _RGB32(0, 0, 0)
    TopLine$ = "Breakpoints: " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(TOTALBREAKPOINTS)))) + TRIM$(STR$(TOTALBREAKPOINTS)) + TAB(5) + "Next line: " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(CLIENT.LINENUMBER)))) + TRIM$(STR$(CLIENT.LINENUMBER)) + " (in " + GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2) + ")"
    _PRINTSTRING (5, (_FONTHEIGHT + 3)), TopLine$
    IF SearchIn = CODE OR SearchIn = LINENUMBERS THEN
        TopLine$ = "Filter (" + IIFSTR$(SearchIn = CODE, "source code", "line numbers") + "): " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
    ELSE
        TopLine$ = "Set next line (must be inside " + GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2) + "): " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
        LINE (0, (_FONTHEIGHT * 2 + 3))-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(255, 255, 0), BF
    END IF
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), TopLine$

    'Top buttons:
    B = 1
    Buttons(B).ID = 1: Buttons(B).CAPTION = "<F5=Run>": B = B + 1
    Buttons(B).ID = 2: Buttons(B).CAPTION = "<F6=Variables>": B = B + 1
    Buttons(B).ID = 3: Buttons(B).CAPTION = "<Trace " + IIFSTR$(TRACE, "ON>", "OFF>"): B = B + 1
    Buttons(B).ID = 4: Buttons(B).CAPTION = IIFSTR$(STEPMODE, IIFSTR$(shiftDown = -1, "<F8=Step Over>", "<F8=Step>"), "<F8=Pause>"): B = B + 1
    IF STEPMODE THEN
        Buttons(B).ID = 8: Buttons(B).CAPTION = "<Set Next>": B = B + 1
        IF LEN(FilteredList$) > 0 THEN
            IF (TOTALBREAKPOINTS > 0 AND shiftDown = -1) OR (TOTALBREAKPOINTS = LEN(FilteredList$) / 4) THEN
                Buttons(B).ID = 6: Buttons(B).CAPTION = "<F10=Clear Breakpoints (filtered)>": B = B + 1
            ELSE
                Buttons(B).ID = 5: Buttons(B).CAPTION = "<F9=Set Breakpoint (filtered)>": B = B + 1
            END IF
        ELSE
            IF TOTALBREAKPOINTS > 0 AND shiftDown = -1 THEN
                Buttons(B).ID = 6: Buttons(B).CAPTION = "<F10=Clear Breakpoints>": B = B + 1
            ELSE
                Buttons(B).ID = 5: Buttons(B).CAPTION = "<F9=Toggle Breakpoint>": B = B + 1
            END IF
        END IF
    ELSE
        IF LEN(FilteredList$) > 0 THEN
            IF (TOTALBREAKPOINTS > 0 AND shiftDown = -1) OR (TOTALBREAKPOINTS = LEN(FilteredList$) / 4) THEN
                Buttons(B).ID = 6: Buttons(B).CAPTION = "<F10=Clear Breakpoints (filtered)>": B = B + 1
            ELSE
                Buttons(B).ID = 5: Buttons(B).CAPTION = "<F9=Set Breakpoint (filtered)>": B = B + 1
            END IF
        ELSE
            IF TOTALBREAKPOINTS > 0 THEN
                Buttons(B).ID = 6: Buttons(B).CAPTION = "<F10=Clear Breakpoints>": B = B + 1
            END IF
        END IF
    END IF
    Buttons(B).ID = 7: Buttons(B).CAPTION = IIFSTR$(LEN(Filter$) > 0, IIFSTR$(SearchIn <> SETNEXT, "<ESC=Clear filter>", "<ESC/TAB=Cancel>"), "<ESC=Exit>"): B = B + 1

    FOR cb = B TO TotalButtons
        Buttons(cb).CAPTION = ""
    NEXT cb

    ButtonLine$ = ""
    FOR cb = 1 TO TotalButtons
        c$ = TRIM$(Buttons(cb).CAPTION)
        ButtonLine$ = ButtonLine$ + IIFSTR$(LEN(c$), c$ + " ", "")
    NEXT cb

    FOR cb = 1 TO TotalButtons
        Buttons(cb).X = INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION)) * _FONTWIDTH + _PRINTWIDTH(ModeTitle$)
        Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
    NEXT cb

    'Show QUICK WATCH panel. -----------------------------------------------------------
    LINE (0, _HEIGHT(MAINSCREEN) - (_FONTHEIGHT * (TOTAL_SELECTEDVARIABLES + 1)))-STEP(_WIDTH(MAINSCREEN), (_FONTHEIGHT * (TOTAL_SELECTEDVARIABLES + 1))), _RGB32(147, 226, 147), BF
    LINE (0, _HEIGHT(MAINSCREEN) - (_FONTHEIGHT * (TOTAL_SELECTEDVARIABLES + 1)))-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT), _RGB32(0, 178, 179), BF
    IF TOTAL_SELECTEDVARIABLES > 0 THEN
        printY = (_HEIGHT(MAINSCREEN) - (_FONTHEIGHT * TOTAL_SELECTEDVARIABLES)) - _FONTHEIGHT
        _PRINTSTRING (5, printY), "QUICK WATCH:"
        this.i = 0
        FOR i = 1 TO CLIENT.TOTALVARIABLES
            IF ASC(SELECTED_VARIABLES, i) > 0 THEN
                this.i = this.i + 1
                v$ = LEFT$(VARIABLES(i).SCOPE, LONGESTSCOPESPEC) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, LONGESTVARNAME) + " = " + TRIM$(VARIABLE_DATA(i).VALUE)
                printY = (_HEIGHT(MAINSCREEN) - (_FONTHEIGHT * TOTAL_SELECTEDVARIABLES)) + (_FONTHEIGHT * this.i) - _FONTHEIGHT
                IF ASC(SELECTED_VARIABLES, i) = 2 THEN
                    'differentiate between pinned quick watch vars and temp quick watch vars
                    LINE (0, printY)-STEP(_WIDTH, _FONTHEIGHT), _RGB32(255, 255, 255), BF
                    LINE (0, printY)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(200, 200, 0, 100), BF
                END IF
                _PRINTSTRING (5, printY), v$
            END IF
        NEXT i
    ELSE
        printY = _HEIGHT(MAINSCREEN) - _FONTHEIGHT
        _PRINTSTRING (5, printY), "QUICK WATCH: No variables added."
    END IF
    '-----------------------------------------------------------------------------------

    GOSUB CheckButtons

    FOR i = 1 TO LEN(ButtonLine$)
        IF (ASC(ButtonLine$, i) <> 60) AND (ASC(ButtonLine$, i) <> 62) THEN
            ASC(ButtonLine$, i) = 32
        END IF
    NEXT i
    COLOR _RGB32(255, 255, 0)
    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 2), ButtonLine$
    COLOR _RGB32(0, 0, 0)

    PCOPY 0, 1

    'Show 'nonexecutable statement' message
    IF ShowTempMessage THEN
        FadeStep# = (TIMER - TempMessage.Start#)
        IF (FadeStep# <= 1.5) THEN
            IF FadeStep# < 1 THEN
                FadeStep# = 0
                FOR popup.Shadow# = 0 TO 5 STEP .5
                    LINE (TempMessage.X + popup.Shadow#, TempMessage.Y + popup.Shadow#)-STEP(TempMessage.W - 1, TempMessage.H - 1), _RGBA32(170, 170, 170, 170 - (34 * popup.Shadow#)), BF
                NEXT popup.Shadow#
            END IF
            LINE (TempMessage.X, TempMessage.Y)-STEP(TempMessage.W - 1, TempMessage.H - 1), _RGBA32(0, 178, 179, 255 - (255 * FadeStep#)), BF
            COLOR _RGBA32(0, 0, 0, 255 - (170 * FadeStep#))
            _PRINTSTRING (TempMessage.X, TempMessage.Y + 4), TempMessage$
            COLOR _RGBA32(255, 255, 255, 255 - (170 * FadeStep#))
            _PRINTSTRING (TempMessage.X - 1, TempMessage.Y + 3), TempMessage$
            COLOR _RGB32(0, 0, 0)
        ELSE
            ShowTempMessage = 0
        END IF
    END IF

    'Show pause icon.
    IF ShowPauseIcon THEN
        PauseFadeStep# = TIMER - SetPause#
        IF (PauseFadeStep# <= .75) THEN
            PauseIconBar.W = 30
            PauseIconBar.H = 100
            PauseIcon.X = _WIDTH / 2 - (PauseIconBar.W * 2.5) / 2
            PauseIcon.Y = _HEIGHT / 2 - PauseIconBar.H / 2

            LINE (PauseIcon.X, PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
            LINE (PauseIcon.X + (PauseIconBar.W * 1.5), PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
        ELSE
            ShowPauseIcon = 0
        END IF
    END IF

    'Show run icon.
    IF ShowRunIcon THEN
        RunFadeStep# = TIMER - SetRun#
        IF (RunFadeStep# <= .75) THEN
            RunIcon.H = 100
            RunIcon.W = 75
            RunIcon.X = _WIDTH / 2 - RunIcon.W / 2
            RunIcon.Y = _HEIGHT / 2 - RunIcon.H / 2

            FOR DrawRunIcon = RunIcon.Y TO RunIcon.Y + RunIcon.H
                LINE (RunIcon.X, DrawRunIcon)-(RunIcon.X + RunIcon.W, RunIcon.Y + RunIcon.H / 2), _RGBA32(0, 178, 179, 255 - (340 * RunFadeStep#))
            NEXT
        ELSE
            ShowRunIcon = 0
        END IF
    END IF

    _DISPLAY
    RETURN

    ColorizeList:
    'Colorize the line if it's the next to be run,...
    IF CLIENT.LINENUMBER = i THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 200), BF
    END IF
    '...if a breakpoint is set,...
    IF ASC(BREAKPOINTLIST, i) = 1 THEN
        BreakpointColor~& = _RGBA32(200, 0, 0, 200)
        IF i = RUNTOTHISLINE THEN BreakpointColor~& = _RGBA32(255, 255, 0, 200)
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT), BreakpointColor~&, BF
    END IF
    '...if "skip this line" is set,...
    IF ASC(BREAKPOINTLIST, i) = 2 THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(255, 255, 0, 200), BF
    END IF
    IF (ShowTempMessage AND TempMessage.printY = printY) THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(255, 255, 0, 255 - (170 * FadeStep#)), BF
    END IF
    RETURN

    DetectClick:
    'Hover:
    IF STEPMODE THEN LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 50), BF

    'Select/Clear the item if a mouse click was detected.
    IF mb THEN
        IF my > _HEIGHT(MAINSCREEN) - (_FONTHEIGHT * (TOTAL_SELECTEDVARIABLES + 1)) THEN
            'Click on QUICK WATCH area takes user to VARIABLE VIEW
            GOSUB ClearTempQuickWatchVars
            VARIABLE_VIEW
            GOSUB AddTempQuickWatchVar
            RETURN
        END IF
        IF OldMXClicked = mx AND OldMYClicked = my AND (TIMER - LastClick# <= 0.5) THEN
            OldMXClicked = -1
            OldMYClicked = -1
            DoubleClick = -1
        ELSE
            OldMXClicked = mx
            OldMYClicked = my
            LastClick# = TIMER
            DoubleClick = 0
        END IF

        'Wait until a mouse up event is received:
        MouseHeld = -1
        WHILE _MOUSEBUTTON(1)
            _LIMIT 500
            SEND_PING
            mb = _MOUSEINPUT
            my = _MOUSEY
            mx = _MOUSEX
        WEND
        MouseHeld = 0
        mb = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click: RETURN

        temp.SourceLine$ = UCASE$(STRIPCOMMENTS$(TRIM$(SourceLine)))
        e1$ = GETELEMENT$(temp.SourceLine$, 1)
        e2$ = GETELEMENT$(temp.SourceLine$, 2)
        PrevDesiredSourceLine$ = " "
        IF i > 1 THEN PrevDesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(i - 1)))
        SELECT CASE e1$
            CASE "DIM", "COMMON", "DATA", "CASE", "TYPE", "REDIM", "CONST", "STATIC", "DEFINT", "DEFLNG", "DEFSTR", "DEFSNG", "DEFDBL", "DECLARE", "_DEFINE", "SUB", "FUNCTION"
                GOSUB TurnOnNonexecutableMessage
            CASE "END"
                IF e2$ = "DECLARE" THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSE
                    GOTO EndAllowed1
                END IF
            CASE "OPTION"
                IF e2$ = "_EXPLICIT" THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSE
                    GOTO EndAllowed1
                END IF
            CASE ELSE
                EndAllowed1:
                IF LEN(temp.SourceLine$) = 0 THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF LEFT$(temp.SourceLine$, 1) = "$" OR LEFT$(temp.SourceLine$, 1) = CHR$(1) THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF RIGHT$(PrevDesiredSourceLine$, 1) = "_" THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF ASC(CHECKINGOFF_LINES, i) THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSE
                    IF DoubleClick THEN
                        ToggleSkipThisLine:
                        DoubleClick = 0
                        IF ASC(BREAKPOINTLIST, i) = 1 THEN TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                        TOTALSKIPLINES = TOTALSKIPLINES + 1
                        ASC(BREAKPOINTLIST, i) = 2
                    ELSE
                        'Toggle breakpoint:
                        IF ASC(BREAKPOINTLIST, i) = 1 AND e1$ <> "STOP" THEN
                            ASC(BREAKPOINTLIST, i) = 0
                            TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                        ELSEIF ASC(BREAKPOINTLIST, i) = 1 AND e1$ = "STOP" THEN
                            Message$ = ""
                            Message$ = Message$ + "The STOP statement creates a permanent breakpoint." + CHR$(LF)
                            Message$ = Message$ + "Would you like to skip this line instead?"
                            IF MESSAGEBOX("Permanent breakpoint", Message$, MKI$(YN_QUESTION), 1, -1) = MB_YES THEN
                                GOTO ToggleSkipThisLine
                            END IF
                        ELSEIF ASC(BREAKPOINTLIST, i) = 2 THEN
                            TOTALSKIPLINES = TOTALSKIPLINES - 1
                            IF e1$ = "STOP" THEN
                                ASC(BREAKPOINTLIST, i) = 1
                                TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                            ELSE
                                ASC(BREAKPOINTLIST, i) = 0
                            END IF
                        ELSE
                            ASC(BREAKPOINTLIST, i) = 1
                            TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                        END IF
                    END IF
                    FOR MultiLineToggle = i + 1 TO CLIENT.TOTALSOURCELINES
                        IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                            ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, i)
                        ELSE
                            EXIT FOR
                        END IF
                    NEXT MultiLineToggle
                END IF
        END SELECT
    END IF

    'Turn on contextual options if right mouse click and while in step mode.
    IF mb2 = -1 AND my < _HEIGHT(MAINSCREEN) - (_FONTHEIGHT * (TOTAL_SELECTEDVARIABLES + 1)) THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(2): _LIMIT 500: SEND_PING: mb2 = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb2 = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click

        temp.SourceLine$ = UCASE$(STRIPCOMMENTS$(TRIM$(SourceLine)))
        e1$ = GETELEMENT$(temp.SourceLine$, 1)
        e2$ = GETELEMENT$(temp.SourceLine$, 2)
        PrevDesiredSourceLine$ = " "
        IF i > 1 THEN PrevDesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(i - 1)))

        SELECT CASE e1$
            CASE "DIM", "COMMON", "DATA", "CASE", "TYPE", "REDIM", "CONST", "STATIC", "DEFINT", "DEFLNG", "DEFSTR", "DEFSNG", "DEFDBL", "DECLARE", "_DEFINE", "SUB", "FUNCTION"
                GenericContextualMenu:
                IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
                    'Set contextual menu with global options only
                    MenuSetup$ = "": MenuID$ = ""
                    MenuSetup$ = MenuSetup$ + IIFSTR$(FIRSTEXECUTION, "R&un", "Contin&ue") + CHR$(LF): MenuID$ = MenuID$ + MKI$(7)
                    IF TOTALBREAKPOINTS > 0 THEN
                        MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                        MenuSetup$ = MenuSetup$ + "&Clear all breakpoints" + CHR$(LF): MenuID$ = MenuID$ + MKI$(5)
                    END IF
                    IF TOTALSKIPLINES > 0 THEN
                        MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                        MenuSetup$ = MenuSetup$ + "Unskip all &lines" + CHR$(LF): MenuID$ = MenuID$ + MKI$(6)
                    END IF

                    Choice = SHOWMENU(MenuSetup$, MenuID$, mx, my)
                    MenuWasInvoked = -1
                    SELECT CASE Choice
                        CASE 5
                            Clicked = -1
                            GOSUB ClearButton_Click
                        CASE 6
                            TOTALSKIPLINES = 0
                            FOR clear.SL = 1 TO CLIENT.TOTALSOURCELINES
                                IF ASC(BREAKPOINTLIST, clear.SL) = 2 THEN
                                    IF GETELEMENT$(GETLINE$(clear.SL), 1) = "STOP" THEN
                                        ASC(BREAKPOINTLIST, clear.SL) = 1
                                        TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                                    ELSE
                                        ASC(BREAKPOINTLIST, clear.SL) = 0
                                    END IF
                                END IF
                            NEXT clear.SL
                        CASE 7
                            Clicked = -1
                            GOSUB RunButton_Click
                    END SELECT
                END IF
            CASE "END"
                IF e2$ = "DECLARE" THEN
                    GOTO GenericContextualMenu
                ELSE
                    GOTO EndAllowed2
                END IF
            CASE ELSE
                EndAllowed2:
                IF LEN(temp.SourceLine$) = 0 THEN
                    GOSUB GenericContextualMenu
                ELSEIF LEFT$(temp.SourceLine$, 1) = "$" OR LEFT$(temp.SourceLine$, 1) = CHR$(1) THEN
                    GOSUB GenericContextualMenu
                ELSEIF RIGHT$(PrevDesiredSourceLine$, 1) = "_" THEN
                    GOSUB GenericContextualMenu
                ELSEIF ASC(CHECKINGOFF_LINES, i) THEN
                    GOSUB GenericContextualMenu
                ELSE
                    IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
                        'Set contextual menu coordinates relative to this item
                        ContextualMenuYRef = y
                        ContextualMenuLineRef = i
                        ContextualMenu.printY = printY
                        ContextualMenu.FilteredList$ = FilteredList$

                        MenuSetup$ = "": MenuID$ = ""
                        MenuSetup$ = MenuSetup$ + IIFSTR$(FIRSTEXECUTION, "R&un", "Contin&ue") + CHR$(LF): MenuID$ = MenuID$ + MKI$(7)
                        MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                        IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2 THEN
                            MenuSetup$ = MenuSetup$ + "~Set &next statement" + CHR$(LF): MenuID$ = MenuID$ + MKI$(1)
                            MenuSetup$ = MenuSetup$ + "~&Run to this line" + CHR$(LF): MenuID$ = MenuID$ + MKI$(3)
                        ELSE
                            MenuSetup$ = MenuSetup$ + "Set &next statement (line " + TRIM$(STR$(ContextualMenuLineRef)) + ")" + CHR$(LF): MenuID$ = MenuID$ + MKI$(1)
                            MenuSetup$ = MenuSetup$ + "&Run to this line" + CHR$(LF): MenuID$ = MenuID$ + MKI$(3)
                            IF INSTR(SOURCECODE(ContextualMenuLineRef), CHR$(5)) > 0 AND CLIENT.LINENUMBER = ContextualMenuLineRef THEN
                                MenuSetup$ = MenuSetup$ + "Step &over" + CHR$(LF): MenuID$ = MenuID$ + MKI$(8)
                            END IF
                        END IF
                        MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                        IF GETELEMENT$(GETLINE$(ContextualMenuLineRef), 1) = "STOP" THEN DisableToggle$ = "~" ELSE DisableToggle$ = ""
                        MenuSetup$ = MenuSetup$ + DisableToggle$ + "Toggle &breakpoint" + CHR$(LF): MenuID$ = MenuID$ + MKI$(2)
                        IF TOTALBREAKPOINTS > 0 THEN
                            MenuSetup$ = MenuSetup$ + "&Clear all breakpoints" + CHR$(LF): MenuID$ = MenuID$ + MKI$(5)
                        END IF
                        MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                        IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2 THEN
                            MenuSetup$ = MenuSetup$ + "Un&skip this line" + CHR$(LF): MenuID$ = MenuID$ + MKI$(4)
                        ELSE
                            MenuSetup$ = MenuSetup$ + "&Skip this line" + CHR$(LF): MenuID$ = MenuID$ + MKI$(4)
                        END IF
                        IF TOTALSKIPLINES > 0 THEN
                            MenuSetup$ = MenuSetup$ + "Unskip all &lines" + CHR$(LF): MenuID$ = MenuID$ + MKI$(6)
                        END IF

                        Choice = SHOWMENU(MenuSetup$, MenuID$, mx, my)
                        MenuWasInvoked = -1
                        SELECT CASE Choice
                            CASE 1
                                'Set next statement
                                Clicked = -1
                                DesiredLine = ContextualMenuLineRef
                                GOSUB SetNext_Click
                            CASE 2
                                'Set breakpoint
                                IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1 AND GETELEMENT$(GETLINE$(ContextualMenuLineRef), 1) <> "STOP" THEN
                                    ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 0
                                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                                ELSE
                                    IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2 THEN TOTALSKIPLINES = TOTALSKIPLINES - 1
                                    ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1
                                    TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                                END IF
                                FOR MultiLineToggle = ContextualMenuLineRef + 1 TO CLIENT.TOTALSOURCELINES
                                    IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                        ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, ContextualMenuLineRef)
                                    ELSE
                                        EXIT FOR
                                    END IF
                                NEXT MultiLineToggle
                            CASE 3
                                'Run to this line
                                IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1 THEN
                                    RUNTOTHISLINE = 0
                                ELSE
                                    ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1
                                    RUNTOTHISLINE = ContextualMenuLineRef
                                    FOR MultiLineToggle = ContextualMenuLineRef + 1 TO CLIENT.TOTALSOURCELINES
                                        IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                            ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, ContextualMenuLineRef)
                                        ELSE
                                            EXIT FOR
                                        END IF
                                    NEXT MultiLineToggle
                                    PUT #FILE, BREAKPOINTLISTBLOCK, BREAKPOINTLIST
                                END IF
                                Clicked = -1
                                GOSUB RunButton_Click
                            CASE 4
                                'Skip this line
                                IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2 THEN
                                    TOTALSKIPLINES = TOTALSKIPLINES - 1
                                    IF GETELEMENT$(GETLINE$(ContextualMenuLineRef), 1) = "STOP" THEN
                                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1
                                        TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                                    ELSE
                                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 0
                                    END IF
                                ELSE
                                    IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1 THEN TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                                    TOTALSKIPLINES = TOTALSKIPLINES + 1
                                    ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2
                                END IF
                                FOR MultiLineToggle = ContextualMenuLineRef + 1 TO CLIENT.TOTALSOURCELINES
                                    IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                        ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, ContextualMenuLineRef)
                                    ELSE
                                        EXIT FOR
                                    END IF
                                NEXT MultiLineToggle
                            CASE 5
                                Clicked = -1
                                GOSUB ClearButton_Click
                            CASE 6
                                TOTALSKIPLINES = 0
                                FOR clear.SL = 1 TO CLIENT.TOTALSOURCELINES
                                    IF ASC(BREAKPOINTLIST, clear.SL) = 2 THEN
                                        IF GETELEMENT$(GETLINE$(clear.SL), 1) <> "STOP" THEN
                                            ASC(BREAKPOINTLIST, clear.SL) = 0
                                        ELSE
                                            ASC(BREAKPOINTLIST, clear.SL) = 1
                                            TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                                        END IF
                                    END IF
                                NEXT clear.SL
                            CASE 7
                                Clicked = -1
                                GOSUB RunButton_Click
                            CASE 8
                                Clicked = -1
                                GOSUB StepOverMenu_Click
                        END SELECT
                    END IF
                END IF
        END SELECT
    END IF
    RETURN

    TurnOnNonexecutableMessage:
    ShowTempMessage = -1
    TempMessageYRef = y
    TempMessage.printY = printY
    IF ASC(CHECKINGOFF_LINES, i) THEN
        TempMessage$ = " $CHECKING/VWATCH64:OFF block (not accessible) "
    ELSEIF LEN(TRIM$(SourceLine)) = 0 THEN
        TempMessage$ = " Blank line "
    ELSEIF RIGHT$(PrevDesiredSourceLine$, 1) = "_" THEN
        TempMessage$ = " Multiline statement "
    ELSE
        TempMessage$ = " Nonexecutable statement "
    END IF
    TempMessage.W = _PRINTWIDTH(TempMessage$) + 6
    TempMessage.H = _FONTHEIGHT * 1.5
    TempMessage.X = mx + _FONTWIDTH * 2: IF TempMessage.X + TempMessage.W > _WIDTH THEN TempMessage.X = _WIDTH - TempMessage.W
    TempMessage.Y = my + _FONTHEIGHT: IF TempMessage.Y + TempMessage.H > _HEIGHT THEN TempMessage.Y = _HEIGHT - TempMessage.H
    TempMessage.Start# = TIMER
    RETURN

    CheckButtons:
    Clicked = 0
    IF my > _FONTHEIGHT THEN _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$: RETURN
    'Hover highlight:
    FOR cb = 1 TO TotalButtons
        IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
            LINE (Buttons(cb).X - 3, 3)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
        END IF
    NEXT cb
    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$

    IF mb THEN
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                WHILE _MOUSEBUTTON(1): _LIMIT 500: SEND_PING: mb = _MOUSEINPUT: WEND
                mb = 0: mx = _MOUSEX: my = _MOUSEY
                'Check if the user moved the mouse out of the button before releasing it (=cancel)
                IF my > _FONTHEIGHT THEN RETURN
                IF (mx < Buttons(cb).X) OR (mx > Buttons(cb).X + Buttons(cb).W) THEN RETURN
                Clicked = -1
                SELECT CASE Buttons(cb).ID
                    CASE 1: GOSUB RunButton_Click
                    CASE 2: GOSUB WindowButton_Click
                    CASE 3: TRACE = NOT TRACE
                    CASE 4: GOSUB StepButton_Click
                    CASE 5: GOSUB ToggleButton_Click
                    CASE 6: GOSUB ClearButton_Click
                    CASE 7: GOSUB ExitButton_Click
                    CASE 8
                        IF SearchIn <> SETNEXT THEN
                            PrevFilter$ = Filter$
                            PrevSearchIn = SearchIn
                            Filter$ = ""
                            SearchIn = SETNEXT
                        ELSE
                            Filter$ = PrevFilter$
                            SearchIn = PrevSearchIn
                        END IF
                    CASE ELSE: SYSTEM_BEEP 0
                END SELECT
            END IF
        NEXT cb
    END IF
    RETURN

    AddTempQuickWatchVar:
    IF VARIABLE_HIGHLIGHT = -1 AND STEPMODE = -1 THEN
        Element = 0
        SourceLine = GETLINE$(CLIENT.LINENUMBER)
        GOSUB ClearTempQuickWatchVars
        MaxTempVars = INT(_HEIGHT / _FONTHEIGHT) - 10
        IF MaxTempVars > 0 THEN
            DO
                Element = Element + 1
                a$ = GETELEMENT$(SourceLine, Element)
                IF a$ = "" THEN EXIT DO
                FOR i = 1 TO CLIENT.TOTALVARIABLES
                    vs$ = TRIM$(VARIABLES(i).NAME)
                    IF INSTR(vs$, "(") THEN vs$ = LEFT$(vs$, INSTR(vs$, "(") - 1)

                    IF UCASE$(a$) = UCASE$(vs$) AND ((TRIM$(UCASE$(VARIABLES(i).SCOPE)) = UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2))) OR TRIM$(VARIABLES(i).SCOPE) = "SHARED") THEN
                        'This element is a variable; add it to quick watch list, if not already there:
                        IF ASC(SELECTED_VARIABLES, i) = 0 THEN
                            ASC(SELECTED_VARIABLES, i) = 2
                            TempQuickWatchVars = TempQuickWatchVars + 1
                            TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES + 1
                            IF TOTAL_SELECTEDVARIABLES >= MaxTempVars THEN EXIT DO
                        END IF
                    END IF
                NEXT i
            LOOP
        END IF
    ELSEIF VARIABLE_HIGHLIGHT = -1 AND STEPMODE = 0 THEN
        GOSUB ClearTempQuickWatchVars
    END IF
    RETURN

    ClearTempQuickWatchVars:
    IF TempQuickWatchVars > 0 THEN
        TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES - TempQuickWatchVars
        TempQuickWatchVars = 0
        FOR i = 1 TO CLIENT.TOTALVARIABLES
            IF ASC(SELECTED_VARIABLES, i) = 2 THEN ASC(SELECTED_VARIABLES, i) = 0
        NEXT
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
FUNCTION GETELEMENT$ (SourceLine$, Element)
    SEP$ = " =<>+-/\^:;,*()" + CHR$(1) + CHR$(3) + CHR$(4) + CHR$(5) + CHR$(6)
    a$ = SourceLine$
    Position = 0
    GETELEMENT_LASTPOSITION = 0
    ThisElement = 0
    DO
        Position = Position + 1
        CommentStart = LEN(STRIPCOMMENTS$(a$))
        IF MID$(a$, Position) = "" THEN EXIT DO
        CheckSep:
        IF INSTR(SEP$, MID$(a$, Position, 1)) > 0 AND Position < LEN(a$) THEN Position = Position + 1: GOTO CheckSep

        Start = Position
        Element$ = ""
        DO
            Char$ = MID$(a$, Position, 1)
            IF INSTR(SEP$, Char$) > 0 THEN EXIT DO
            Element$ = Element$ + Char$
            Position = Position + 1
            IF Position > LEN(a$) THEN EXIT DO
        LOOP
        ThisElement = ThisElement + 1
        IF (LEFT$(Element$, 1) = CHR$(34) AND LEN(Element$) > 1 AND RIGHT$(Element$, 1) <> CHR$(34)) _
            OR Element$ = CHR$(34) THEN
            'We found an open quote.
            'This element will go up until the next closing quote, if any
            ClosingQuote = INSTR(Position, a$, CHR$(34))
            IF ClosingQuote > 0 THEN
                Element$ = Element$ + MID$(a$, Position, ClosingQuote - Position + 1)
                Position = ClosingQuote + 1
            END IF
        END IF
        IF ThisElement = Element THEN GETELEMENT$ = Element$: GETELEMENT_LASTPOSITION = Position: EXIT DO
    LOOP
END FUNCTION

'------------------------------------------------------------------------------
SUB ADDCOLORCODE (SourceLineNumber)
    'Add colorization code
    SEP$ = " =<>+-/\^:;,*()"
    i = SourceLineNumber
    Position = 0
    InQuote = 0

    DO
        Position = Position + 1
        a$ = UCASE$(SOURCECODE(i))
        CommentStart = LEN(STRIPCOMMENTS$(a$))
        IF CommentStart = LEN(a$) THEN CommentStart = CommentStart + 1

        IF MID$(a$, Position) = "" THEN EXIT DO
        CheckSep:
        IF INSTR(SEP$, MID$(a$, Position, 1)) > 0 AND Position < LEN(a$) THEN Position = Position + 1: GOTO CheckSep
        IF ASC(a$, Position) = 34 THEN InQuote = NOT InQuote

        Start = Position
        Element$ = ""
        DO
            Char$ = MID$(a$, Position, 1)
            IF INSTR(SEP$, Char$) > 0 THEN EXIT DO
            Element$ = Element$ + Char$
            Position = Position + 1
            IF Position > LEN(a$) THEN EXIT DO
        LOOP
        a$ = Element$
        IF LEN(a$) = 0 THEN EXIT DO
        ColorCode$ = ""

        'Check if it's a QB64 keyword:
        FOR j = 1 TO UBOUND(QB64KEYWORDS)
            ThisKW$ = QB64KEYWORDS(j)
            IF a$ = ThisKW$ THEN
                IF LEFT$(ThisKW$, 1) = "$" THEN
                    IF NOT InQuote THEN ColorCode$ = CHR$(1)
                ELSE
                    IF Start < CommentStart AND InQuote = 0 THEN ColorCode$ = CHR$(3)
                END IF
                IF LEN(ColorCode$) > 0 THEN GOSUB AddThisColorCode
                EXIT FOR
            END IF
        NEXT j

        'Check if it's a SUB/FUNCTION
        IF ColorCode$ = "" THEN
            FOR j = 1 TO UBOUND(SUBFUNC)
                ThisKW$ = REMOVESIGIL$(UCASE$(TRIM$(SUBFUNC(j).NAME)))
                IF REMOVESIGIL$(a$) = ThisKW$ THEN
                    IF Start < CommentStart AND InQuote = 0 THEN
                        IF SUBFUNC(j).EXTERNAL THEN ColorCode$ = CHR$(6) ELSE ColorCode$ = CHR$(5)
                        GOSUB AddThisColorCode
                    END IF
                    EXIT FOR
                END IF
            NEXT j
        END IF

        'Check if it's a numeric value
        IF ColorCode$ = "" THEN
            SELECT CASE ASC(Element$, 1)
                CASE 48 TO 57
                    'Fine, it's numerical.
                    CheckNegative:
                    IF Start > 1 THEN
                        'Is it a negative?
                        IF ASC(SOURCECODE(i), Start - 1) = 45 THEN
                            'Yup. Include the minus sign.
                            Start = Start - 1
                        END IF
                    END IF
                    IF Start < CommentStart AND NOT InQuote THEN ColorCode$ = CHR$(4)
                CASE 46
                    'Periods are fine as long as the next character is numeric
                    IF LEN(Element$) > 1 THEN
                        SELECT CASE ASC(Element$, 2)
                            CASE 48 TO 57
                                'Fine, it's numerical.
                                GOTO CheckNegative
                        END SELECT
                    END IF
                CASE ELSE
                    ColorCode$ = ""
            END SELECT
            IF LEN(ColorCode$) > 0 THEN GOSUB AddThisColorCode
        END IF
    LOOP

    SOURCECODE_COLORIZED(i) = -1

    EXIT SUB
    AddThisColorCode:
    IF Start > 1 THEN
        SOURCECODE(i) = LEFT$(SOURCECODE(i), Start - 1) + ColorCode$ + MID$(SOURCECODE(i), Start)
    ELSE
        SOURCECODE(i) = ColorCode$ + SOURCECODE(i)
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
SUB PRINT_COLORIZED (StartX AS INTEGER, Y AS INTEGER, v$, SourceLineNumber AS LONG)
    DIM InQuote AS _BYTE
    DIM MetaCommand AS LONG
    DIM IsNumber AS LONG
    DIM IsSubFunc AS LONG
    DIM IsExtSubFunc AS LONG
    DIM CommentStart AS LONG

    SEP$ = " =<>+-/\^:;,*()"

    CommentStart = LEN(STRIPCOMMENTS$(v$))

    DO
        Position = Position + 1
        'Lines with breakpoints, skip flags or in $CHECKING/VWATCH:OFF blocks are printed in white/gray
        IF RUNTOTHISLINE = SourceLineNumber THEN COLOR _RGB32(0, 0, 0): GOTO ColorSet
        IF ASC(BREAKPOINTLIST, SourceLineNumber) = 1 THEN COLOR _RGB32(255, 255, 255): GOTO ColorSet
        IF ASC(BREAKPOINTLIST, SourceLineNumber) = 2 THEN COLOR _RGB32(180, 180, 180): GOTO ColorSet
        IF ASC(CHECKINGOFF_LINES, SourceLineNumber) AND Position >= LINE_TRAIL THEN COLOR _RGB32(170, 170, 170): GOTO ColorSet

        IF ASC(v$, Position) = 34 OR InQuote THEN
            'Text in "quotation marks"
            IF ASC(v$, Position) = 34 THEN InQuote = NOT InQuote
            COLOR _RGB32(255, 165, 0)
            GOTO ColorSet
        END IF
        IF INSTR(SEP$, MID$(v$, Position, 1)) > 0 AND (MetaCommand > 0 OR KeyWord > 0 OR IsNumber > 0 OR IsSubFunc > 0 OR IsExtSubFunc > 0) AND NOT InQuote THEN
            IF KeyWord > 0 THEN KeyWord = 0
            IF IsSubFunc > 0 THEN IsSubFunc = 0
            IF IsExtSubFunc > 0 THEN IsExtSubFunc = 0
            IF MID$(v$, Position, 1) = "-" AND IsNumber > 0 THEN
                IF Position = IsNumber + 1 THEN
                    'It's a negative.
                ELSE
                    'It's a separator.
                    IsNumber = 0
                END IF
            ELSEIF IsNumber > 0 THEN
                IsNumber = 0
            END IF
        END IF
        IF (ASC(v$, Position) = 1 OR MetaCommand > 0) AND NOT InQuote THEN
            IF ASC(v$, Position) = 1 THEN MetaCommand = Position
            COLOR _RGB32(46, 160, 87)
            GOTO ColorSet
        END IF
        IF (ASC(v$, Position) = 3 OR KeyWord > 0) AND NOT InQuote THEN
            IF ASC(v$, Position) = 3 THEN KeyWord = Position
            COLOR _RGB32(0, 0, 255)
            GOTO ColorSet
        END IF
        IF (ASC(v$, Position) = 4 OR IsNumber > 0) AND NOT InQuote THEN
            IF ASC(v$, Position) = 4 THEN IsNumber = Position
            COLOR _RGB32(255, 0, 0)
            GOTO ColorSet
        END IF
        IF (ASC(v$, Position) = 5 OR IsSubFunc > 0) AND NOT InQuote THEN
            IF ASC(v$, Position) = 5 THEN IsSubFunc = Position
            COLOR _RGB32(255, 0, 255)
            GOTO ColorSet
        END IF
        IF (ASC(v$, Position) = 6 OR IsExtSubFunc > 0) AND NOT InQuote THEN
            IF ASC(v$, Position) = 6 THEN IsExtSubFunc = Position
            COLOR _RGB32(153, 77, 0)
            GOTO ColorSet
        END IF
        COLOR _RGB32(0, 0, 0)

        ColorSet:
        GOSUB PrintChar
    LOOP UNTIL Position = CommentStart

    IF CommentStart < LEN(v$) THEN
        FoundMetaCommand = INSTR(CommentStart, v$, CHR$(1))
        IF FoundMetaCommand > 0 THEN
            COLOR _RGB32(170, 170, 170)
            _PRINTSTRING (StartX + (_FONTWIDTH * (Position - 1)), Y), MID$(v$, Position, FoundMetaCommand - Position)
            COLOR _RGB32(46, 160, 87)
            _PRINTSTRING (StartX + (_FONTWIDTH * (FoundMetaCommand - 1)), Y), MID$(v$, FoundMetaCommand + 1)
        ELSE
            COLOR _RGB32(170, 170, 170)
            _PRINTSTRING (StartX + (_FONTWIDTH * (Position - 1)), Y), MID$(v$, Position)
        END IF
    END IF

    EXIT SUB
    PrintChar:
    Char = ASC(v$, Position)
    IF (Char = 1 OR Char = 3 OR Char = 4 OR Char = 5 OR Char = 6) AND NOT InQuote THEN
        StartX = StartX - _FONTWIDTH
    ELSE
        _PRINTSTRING (StartX + (_FONTWIDTH * (Position - 1)), Y), CHR$(Char)
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
SUB VARIABLE_VIEW
    DIM SB_Ratio AS SINGLE
    DIM SourceLine AS STRING
    DIM ListEnd_Label AS STRING
    DIM LastToggledON AS LONG
    STATIC Filter$
    STATIC SearchIn
    STATIC y AS LONG

    TotalButtons = 8
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS , _RGB32(255, 255, 255)

    IF SearchIn = 0 THEN SearchIn = VARIABLENAMES
    SB_ThumbY = 0
    grabbedY = -1
    ListEnd_Label = "(end of list)"
    _KEYCLEAR

    SWITCH_VIEW = 0

    DO: _LIMIT 500
        GOSUB ProcessInput

        SEND_PING

        GET #FILE, CLIENTBLOCK, CLIENT
        FIND_CURRENTMODULE
        PUT #FILE, WATCHPOINTLISTBLOCK, WATCHPOINTLIST
        PUT #FILE, WATCHPOINTEXPBLOCK, WATCHPOINT()
        GET #FILE, DATABLOCK, VARIABLE_DATA()
        GET #FILE, WATCHPOINTCOMMANDBLOCK, WATCHPOINT_COMMAND

        IF LEN(SOURCEFILE) > 0 THEN
            FOR i = 1 TO CLIENT.TOTALVARIABLES
                IF (TRIM$(UCASE$(VARIABLES(i).SCOPE)) <> UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2))) AND TRIM$(VARIABLES(i).SCOPE) <> "SHARED" THEN
                    VARIABLE_DATA(i).VALUE = "<out of scope>"
                END IF
            NEXT i
        END IF

        IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN STEPMODE = -1
        IF WATCHPOINT_COMMAND.ACTION = NEXTSTEP THEN
            WATCHPOINTBREAK = WATCHPOINT_COMMAND.LINENUMBER
            STEPMODE = -1
            SetPause# = TIMER
            ShowPauseIcon = -1
            ShowRunIcon = 0
            WATCHPOINT_COMMAND.ACTION = READY
            WATCHPOINT_COMMAND.LINENUMBER = 0
            PUT #FILE, WATCHPOINTCOMMANDBLOCK, WATCHPOINT_COMMAND
        END IF

        GOSUB UpdateList

        IF _EXIT THEN USERQUIT = -1
    LOOP UNTIL USERQUIT OR CLOSE_SESSION OR SWITCH_VIEW OR HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED

    EXIT SUB
    ProcessInput:
    k = _KEYHIT: modKey = k
    IF modKey = 100303 OR modKey = 100304 THEN shiftDown = -1
    IF modKey = -100303 OR modKey = -100304 THEN shiftDown = 0
    IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
    IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0

    DO
        y = y + (_MOUSEWHEEL * ((_HEIGHT - 50) / 5))
        mx = _MOUSEX
        my = _MOUSEY
        mb = _MOUSEBUTTON(1)
        mb2 = _MOUSEBUTTON(2)
    LOOP WHILE _MOUSEINPUT

    IF my < SCREEN_TOPBAR AND (ShowPopupWatchpoint) THEN ShowPopupWatchpoint = 0

    SELECT EVERYCASE k
        CASE 86, 118 'V
            IF ctrlDown = -1 THEN
                IF LEN(_CLIPBOARD$) THEN Filter$ = Filter$ + _CLIPBOARD$
                k = 0
            END IF
        CASE 32 TO 126 'Printable ASCII characters
            Filter$ = Filter$ + CHR$(k)
        CASE 8 'Backspace
            IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
        CASE 9, 25 'TAB alternates between what is filtered (VARIABLENAMES, DATATYPES)
            IF SearchIn = SCOPE THEN Filter$ = ""
            SELECT CASE SearchIn
                CASE SCOPE: IF shiftDown = 0 THEN SearchIn = DATATYPES ELSE SearchIn = VALUES
                CASE DATATYPES: IF shiftDown = 0 THEN SearchIn = VARIABLENAMES ELSE SearchIn = SCOPE
                CASE VARIABLENAMES: IF shiftDown = 0 THEN SearchIn = VALUES ELSE SearchIn = DATATYPES
                CASE VALUES: IF shiftDown = 0 THEN SearchIn = SCOPE ELSE SearchIn = VARIABLENAMES
            END SELECT
        CASE 27 'ESC clears the current search filter or exits MONITOR_MODE
            ExitButton_Click:
            IF LEN(Filter$) THEN
                Filter$ = ""
            ELSE
                CLOSE_SESSION = -1
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 18432 'Up
            ArrowStep! = ((_HEIGHT - 50) * SB_Ratio)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF PAGE_HEIGHT > LIST_AREA THEN
                IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - ArrowStep!
            END IF
        CASE 20480 'Down
            ArrowStep! = ((_HEIGHT - 50) * SB_Ratio)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF PAGE_HEIGHT > LIST_AREA THEN
                IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + ArrowStep!
            END IF
        CASE 16128 'F5
            RunButton_Click:
            FIRSTEXECUTION = 0
            IF WATCHPOINTBREAK > 0 THEN
                IF ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 1 THEN
                    Message$ = "Execution was halted on a watchpoint (" + TRIM$(VARIABLES(WATCHPOINTBREAK).NAME) + TRIM$(WATCHPOINT(WATCHPOINTBREAK).EXPRESSION) + ")" + CHR$(LF)
                    Message$ = Message$ + "Clear it before resuming?"
                    MESSAGEBOX_RESULT = MESSAGEBOX("Run/Resume", Message$, MKI$(YN_QUESTION), 1, -1)
                    IF MESSAGEBOX_RESULT = MB_YES THEN
                        ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 0
                        TOTALWATCHPOINTS = TOTALWATCHPOINTS - 1
                        WATCHPOINT(WATCHPOINTBREAK).EXPRESSION = ""
                        PUT #FILE, WATCHPOINTLISTBLOCK, WATCHPOINTLIST
                        PUT #FILE, WATCHPOINTEXPBLOCK, WATCHPOINT()
                    END IF
                END IF
            END IF
            IF STEPMODE = -1 THEN SetRun# = TIMER: ShowRunIcon = -1
            STEPMODE = 0
            WATCHPOINTBREAK = 0
            BREAKPOINT.ACTION = CONTINUE
            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16384 'F6
            WindowButton_Click:
            _KEYCLEAR
            SWITCH_VIEW = -1
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16896 'F8
            StepButton_Click:
            FIRSTEXECUTION = 0
            IF shiftDown THEN
                IF SOURCECODE_COLORIZED(CLIENT.LINENUMBER) = 0 THEN ADDCOLORCODE CLIENT.LINENUMBER
                IF INSTR(SOURCECODE(CLIENT.LINENUMBER), CHR$(5)) > 0 THEN
                    IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                    STEPMODE = -1
                    TRACE = -1
                    BREAKPOINT.ACTION = SKIPSUB
                    PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
                ELSE
                    GOTO NormalF8
                END IF
            ELSE
                NormalF8:
                IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                STEPMODE = -1
                TRACE = -1
                BREAKPOINT.ACTION = NEXTSTEP
                PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 17152 'F9
            ToggleButton_Click:
            IF shiftDown = -1 THEN GOTO ClearButton_CLICK
            IF STEPMODE = -1 THEN
                IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 AND GETELEMENT$(GETLINE$(CLIENT.LINENUMBER), 1) <> "STOP" THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                ELSEIF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 AND GETELEMENT$(GETLINE$(CLIENT.LINENUMBER), 1) = "STOP" THEN
                    Message$ = ""
                    Message$ = Message$ + "The STOP statement creates a permanent breakpoint." + CHR$(LF)
                    Message$ = Message$ + "Would you like to skip this line instead?"
                    IF MESSAGEBOX("Permanent breakpoint", Message$, MKI$(YN_QUESTION), 1, -1) = MB_YES THEN
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                        TOTALSKIPLINES = TOTALSKIPLINES + 1
                        ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 2
                    END IF
                ELSE
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                END IF
                FOR MultiLineToggle = CLIENT.LINENUMBER + 1 TO CLIENT.TOTALSOURCELINES
                    IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                        ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, CLIENT.LINENUMBER)
                    ELSE
                        EXIT FOR
                    END IF
                NEXT MultiLineToggle
                PUT #FILE, BREAKPOINTLISTBLOCK, BREAKPOINTLIST
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 17408 'F10
            ClearButton_CLICK:
            FOR clear.BP = 1 TO CLIENT.TOTALSOURCELINES
                IF ASC(BREAKPOINTLIST, clear.BP) = 1 AND GETELEMENT$(GETLINE$(clear.BP), 1) <> "STOP" THEN ASC(BREAKPOINTLIST, clear.BP) = 0: totalbreapoints = TOTALBREAKPOINTS - 1
            NEXT clear.BP
            PUT #FILE, BREAKPOINTLISTBLOCK, BREAKPOINTLIST
            IF Clicked THEN Clicked = 0: RETURN
    END SELECT

    'Scrollbar check:
    IF PAGE_HEIGHT > LIST_AREA THEN
        IF mb THEN
            IF mx > _WIDTH(MAINSCREEN) - 30 AND mx < _WIDTH(MAINSCREEN) THEN
                'Clicked inside the scroll bar. Check if click was on the thumb:
                IF my > SCREEN_TOPBAR + SB_ThumbY + 24 AND my < SCREEN_TOPBAR + SB_ThumbY + 24 + SB_ThumbH THEN
                    'Clicked on the thumb:
                    grabbedY = my: starty = y
                    DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
                    DO WHILE _MOUSEBUTTON(1): _LIMIT 500
                        WHILE _MOUSEINPUT: WEND
                        my = _MOUSEY
                        y = starty + ((my - grabbedY) / SB_Ratio)

                        CHECK_SCREEN_LIMITS y
                        IF prevY <> y THEN
                            DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my: prevY = y
                            GOSUB UpdateList
                        END IF
                        SEND_PING
                        _DISPLAY
                    LOOP
                    grabbedY = -1
                ELSEIF my > SCREEN_TOPBAR AND my <= SCREEN_TOPBAR + 20 THEN
                    'Up arrow
                    IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - (LIST_AREA / 10)
                    _DELAY .1
                ELSEIF my > SCREEN_HEIGHT - 21 THEN
                    'Down arrow
                    IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + (LIST_AREA / 10)
                    _DELAY .1
                ELSE
                    'Clicked above or below the thumb:
                    IF my < SCREEN_TOPBAR + 25 + SB_ThumbY AND my > SCREEN_TOPBAR + 21 THEN
                        y = y - ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    ELSEIF my > SCREEN_TOPBAR + 25 + SB_ThumbY + SB_ThumbH AND my < SCREEN_HEIGHT - 21 THEN
                        y = y + ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    END IF
                END IF
            END IF
        END IF
    END IF
    RETURN

    UpdateList:
    CHECK_RESIZE 0, 0
    LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR
    CLS , _RGB32(255, 255, 255)
    cursorBlink% = cursorBlink% + 1
    IF cursorBlink% > 50 THEN cursorBlink% = 0

    'Build a filtered list, if a filter is active
    i = 0: FilteredList$ = ""
    PAGE_HEIGHT = _FONTHEIGHT * (CLIENT.TOTALVARIABLES + 3)
    IF LEN(Filter$) > 0 THEN
        DO
            i = i + 1
            IF i > CLIENT.TOTALVARIABLES THEN EXIT DO
            Found = 0
            SELECT CASE SearchIn
                CASE VARIABLENAMES: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).NAME), UCASE$(Filter$))
                CASE DATATYPES: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).DATATYPE), UCASE$(Filter$))
                CASE VALUES: Found = MULTI_SEARCH(UCASE$(VARIABLE_DATA(i).VALUE), UCASE$(Filter$))
                CASE SCOPE: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).SCOPE), UCASE$(Filter$))
            END SELECT
            IF Found THEN
                FilteredList$ = FilteredList$ + MKL$(i)
            END IF
        LOOP
        IF LEN(FilteredList$) > 0 THEN PAGE_HEIGHT = _FONTHEIGHT * ((LEN(FilteredList$) / 4) + 3)
    END IF

    CHECK_SCREEN_LIMITS y

    'Place a light gray rectangle under the column that can currently be filtered
    SELECT CASE SearchIn
        CASE DATATYPES
            columnHighlightX = _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 1))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(21)) + _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 1))
            columnHighlightW = _PRINTWIDTH(SPACE$(LONGESTVARNAME)) + 8
        CASE VALUES
            columnHighlightX = _PRINTWIDTH(SPACE$(LONGESTVARNAME)) + _PRINTWIDTH(SPACE$(20)) + _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 1)) + 16
            columnHighlightW = _WIDTH
        CASE SCOPE
            columnHighlightX = 0
            columnHighlightW = _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC)) + 8
    END SELECT
    columnHighlightY = 51

    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        columnHightlighH = (LEN(FilteredList$) / 4 * _FONTHEIGHT) + _FONTHEIGHT
    ELSEIF LEN(Filter$) > 0 AND LEN(FilteredList$) = 0 THEN
        columnHightlighH = 0
    ELSE
        columnHightlighH = (CLIENT.TOTALVARIABLES * _FONTHEIGHT) + _FONTHEIGHT
    END IF
    CLS , _RGB32(255, 255, 255)
    LINE (columnHighlightX, columnHighlightY)-STEP(columnHighlightW, columnHightlighH), _RGB32(230, 230, 230), BF

    'Print list items to the screen:
    SourceLine = TRIM$(GETLINE$(CLIENT.LINENUMBER))
    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        FOR ii = ((y \ _FONTHEIGHT) + 1) TO LEN(FilteredList$) / 4
            i = CVL(MID$(FilteredList$, ii * 4 - 3, 4))
            v$ = LEFT$(VARIABLES(i).SCOPE, LONGESTSCOPESPEC) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, LONGESTVARNAME) + " = " + TRIM$(VARIABLE_DATA(i).VALUE)
            printY = ((3 + ii) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            IF MenuWasInvoked THEN MenuWasInvoked = 0: RETURN
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO CLIENT.TOTALVARIABLES
            v$ = LEFT$(VARIABLES(i).SCOPE, LONGESTSCOPESPEC) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, LONGESTVARNAME) + " = " + TRIM$(VARIABLE_DATA(i).VALUE)
            printY = ((3 + i) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            IF MenuWasInvoked THEN MenuWasInvoked = 0: RETURN
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT i
    END IF
    COLOR _RGB32(0, 0, 0)

    IF LEN(Filter$) AND LEN(FilteredList$) = 0 THEN 'A filter is on, but nothing was found
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT), "Not found."
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to clear)"
    END IF

    IF PAGE_HEIGHT > LIST_AREA THEN
        IF LEN(Filter$) AND LEN(FilteredList$) > 0 THEN
            _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
        ELSEIF LEN(Filter$) = 0 THEN
            _PRINTSTRING (5, ((5 + CLIENT.TOTALVARIABLES) * _FONTHEIGHT) - y), ListEnd_Label
        END IF
        DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
    ELSE
        'End of list message:
        IF LEN(Filter$) AND LEN(FilteredList$) > 0 THEN
            _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
        ELSEIF LEN(Filter$) = 0 THEN
            _PRINTSTRING (5, PAGE_HEIGHT + (_FONTHEIGHT * 2) - y), ListEnd_Label
        END IF
    END IF

    'Top bar:
    '  VARIABLE VIEW: <F5 = Run> <F6 = View Source> <F8 = Step> <F9 = Toggle Breakpoint> <ESC = Exit>
    '  Next line: ####
    '  Filter:                                                              Total variables: 10 (showing 7)
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), SCREEN_TOPBAR), _RGB32(102, 255, 102), BF
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    ModeTitle$ = "VARIABLE VIEW: "
    _PRINTSTRING (5, 3), ModeTitle$
    COLOR _RGB32(255, 255, 255)
    _PRINTSTRING (4, 2), ModeTitle$
    COLOR _RGB32(0, 0, 0)
    TopLine$ = GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)
    _PRINTSTRING (_WIDTH - 3 - _PRINTWIDTH(TopLine$), 3), TopLine$
    TopLine$ = "Total variables:" + STR$(CLIENT.TOTALVARIABLES) + IIFSTR$(LEN(FilteredList$), " (showing " + TRIM$(STR$(LEN(FilteredList$) / 4)) + ")", "")
    _PRINTSTRING (_WIDTH - 5 - _PRINTWIDTH(TopLine$), (_FONTHEIGHT * 2 + 3)), TopLine$
    TopLine$ = "Next line: "
    _PRINTSTRING (3, _FONTHEIGHT + 3), TopLine$
    tl.x = 3 + _PRINTWIDTH(TopLine$)
    IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
        LINE (tl.x, _FONTHEIGHT + 3)-STEP(_WIDTH, _FONTHEIGHT), _RGB32(200, 0, 0), BF
    ELSEIF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 2 THEN
        LINE (tl.x, _FONTHEIGHT + 3)-STEP(_WIDTH, _FONTHEIGHT), _RGB32(255, 255, 0), BF
    END IF
    IF LEN(SOURCEFILE) THEN IF SOURCECODE_COLORIZED(CLIENT.LINENUMBER) = 0 THEN ADDCOLORCODE CLIENT.LINENUMBER
    TopLine$ = SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(CLIENT.LINENUMBER)))) + TRIM$(STR$(CLIENT.LINENUMBER)) + " " + CHR$(16) + " " + SourceLine
    IF LEN(SOURCEFILE) THEN PRINT_COLORIZED tl.x, _FONTHEIGHT + 3, TopLine$, CLIENT.LINENUMBER
    COLOR _RGB32(0, 0, 0)
    TopLine$ = "Filter: " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), TopLine$

    'Top buttons:
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F5=Run>": b = b + 1
    Buttons(b).ID = 2: Buttons(b).CAPTION = "<F6=Source>": b = b + 1
    Buttons(b).ID = 3: Buttons(b).CAPTION = IIFSTR$(STEPMODE, IIFSTR$(shiftDown = -1, "<F8=Step Over>", "<F8=Step>"), "<F8=Pause>"): b = b + 1
    Buttons(b).ID = 7: Buttons(b).CAPTION = "<Highlight " + IIFSTR$(VARIABLE_HIGHLIGHT, "ON>", "OFF>"): b = b + 1
    IF STEPMODE THEN
        IF TOTALBREAKPOINTS > 0 AND shiftDown = -1 THEN
            Buttons(b).ID = 5: Buttons(b).CAPTION = "<F10=Clear Breakpoints>": b = b + 1
        ELSE
            Buttons(b).ID = 4: Buttons(b).CAPTION = "<F9=Toggle Breakpoint>": b = b + 1
        END IF
    ELSE
        Buttons(b).ID = 4: Buttons(b).CAPTION = "": b = b + 1
        Buttons(b).ID = 5: Buttons(b).CAPTION = "": b = b + 1
    END IF
    Buttons(b).ID = 6: Buttons(b).CAPTION = IIFSTR$(LEN(Filter$) > 0, "<ESC=Clear filter>", "<ESC=Exit>"): b = b + 1

    FOR cb = b TO TotalButtons
        Buttons(cb).CAPTION = ""
    NEXT cb

    ButtonLine$ = ""
    FOR cb = 1 TO TotalButtons
        c$ = TRIM$(Buttons(cb).CAPTION)
        ButtonLine$ = ButtonLine$ + IIFSTR$(LEN(c$), c$ + " ", "")
    NEXT cb

    FOR cb = 1 TO TotalButtons
        Buttons(cb).X = INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION)) * _FONTWIDTH + _PRINTWIDTH(ModeTitle$)
        Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
    NEXT cb

    GOSUB CheckButtons
    IF SWITCH_VIEW THEN RETURN

    FOR i = 1 TO LEN(ButtonLine$)
        IF (ASC(ButtonLine$, i) <> 60) AND (ASC(ButtonLine$, i) <> 62) THEN
            ASC(ButtonLine$, i) = 32
        END IF
    NEXT i
    COLOR _RGB32(255, 255, 0)
    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 2), ButtonLine$
    COLOR _RGB32(0, 0, 0)

    PCOPY 0, 1

    'Show watchpoint hover popup
    IF ShowPopupWatchpoint THEN
        FOR popup.Shadow# = 0 TO 5 STEP .5
            LINE (PopupWatchpoint.X + popup.Shadow#, PopupWatchpoint.Y + popup.Shadow#)-STEP(PopupWatchpoint.W - 1, PopupWatchpoint.H - 1), _RGBA32(170, 170, 170, 170 - (34 * popup.Shadow#)), BF
        NEXT popup.Shadow#
        LINE (PopupWatchpoint.X, PopupWatchpoint.Y)-STEP(PopupWatchpoint.W - 1, PopupWatchpoint.H - 1), _RGB32(0, 178, 179), BF
        _PRINTSTRING (PopupWatchpoint.X, PopupWatchpoint.Y + 4), WatchpointPopup$
    END IF

    'Show pause icon.
    IF ShowPauseIcon THEN
        PauseFadeStep# = TIMER - SetPause#
        IF (PauseFadeStep# <= .75) THEN
            PauseIconBar.W = 30
            PauseIconBar.H = 100
            PauseIcon.X = _WIDTH / 2 - (PauseIconBar.W * 2.5) / 2
            PauseIcon.Y = _HEIGHT / 2 - PauseIconBar.H / 2

            LINE (PauseIcon.X, PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
            LINE (PauseIcon.X + (PauseIconBar.W * 1.5), PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
        ELSE
            ShowPauseIcon = 0
        END IF
    END IF

    'Show run icon.
    IF ShowRunIcon THEN
        RunFadeStep# = TIMER - SetRun#
        IF (RunFadeStep# <= .75) THEN
            RunIcon.H = 100
            RunIcon.W = 75
            RunIcon.X = _WIDTH / 2 - RunIcon.W / 2
            RunIcon.Y = _HEIGHT / 2 - RunIcon.H / 2

            FOR DrawRunIcon = RunIcon.Y TO RunIcon.Y + RunIcon.H
                LINE (RunIcon.X, DrawRunIcon)-(RunIcon.X + RunIcon.W, RunIcon.Y + RunIcon.H / 2), _RGBA32(0, 178, 179, 255 - (340 * RunFadeStep#))
            NEXT
        ELSE
            ShowRunIcon = 0
        END IF
    END IF

    _DISPLAY
    RETURN

    ColorizeSelection:
    'Indicate that this variable is used in the current source line
    IF VARIABLE_HIGHLIGHT = -1 THEN
        vs$ = TRIM$(VARIABLES(i).NAME)
        IF INSTR(vs$, "(") THEN vs$ = LEFT$(vs$, INSTR(vs$, "(") - 1)
        Element = 0
        DO
            Element = Element + 1
            a$ = GETELEMENT$(SourceLine, Element)
            IF a$ = "" THEN EXIT DO
            IF UCASE$(a$) = UCASE$(vs$) AND ((TRIM$(UCASE$(VARIABLES(i).SCOPE)) = UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2))) OR TRIM$(VARIABLES(i).SCOPE) = "SHARED") THEN
                LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 0, 100), BF
                EXIT DO
            END IF
        LOOP
    END IF

    COLOR _RGB(0, 0, 0)

    'or that it has a watchpoint set
    IF ASC(WATCHPOINTLIST, i) = 1 THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(255, 0, 0, 200), BF
        COLOR _RGB(255, 255, 255)
    END IF

    'or that it's been selected to be watched in source view
    IF ASC(SELECTED_VARIABLES, i) = 1 THEN
        SelectedItemColor~& = _RGBA32(0, 200, 0, 100)
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), SelectedItemColor~&, BF
    END IF

    RETURN

    DetectClick:
    'Hover/Watchpoint popup:
    IF STEPMODE THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 50), BF

        IF ASC(WATCHPOINTLIST, i) = 1 THEN
            ShowPopupWatchpoint = -1
            WatchpointPopup$ = " Watchpoint: " + TRIM$(VARIABLES(i).NAME) + TRIM$(WATCHPOINT(i).EXPRESSION) + " "
            DO
                WatchpointPopup$ = LEFT$(WatchpointPopup$, LEN(WatchpointPopup$) - 1)
                PopupWatchpoint.W = _PRINTWIDTH(WatchpointPopup$) + 6
            LOOP UNTIL PopupWatchpoint.W < _WIDTH

            PopupWatchpoint.H = _FONTHEIGHT * 1.5
            PopupWatchpoint.X = mx + _FONTWIDTH * 2: IF PopupWatchpoint.X + PopupWatchpoint.W > _WIDTH THEN PopupWatchpoint.X = _WIDTH - PopupWatchpoint.W
            PopupWatchpoint.Y = my + _FONTHEIGHT: IF PopupWatchpoint.Y + PopupWatchpoint.H > _HEIGHT THEN PopupWatchpoint.Y = _HEIGHT - PopupWatchpoint.H
        ELSE
            ShowPopupWatchpoint = 0
        END IF
    END IF

    IF mb THEN
        IF OldMXClicked = mx AND OldMYClicked = my AND (TIMER - LastClick# <= 0.5) THEN
            OldMXClicked = -1
            OldMYClicked = -1
            DoubleClick = -1
        ELSE
            OldMXClicked = mx
            OldMYClicked = my
            LastClick# = TIMER
            DoubleClick = 0
        END IF

        'Wait until a mouse up event is received:
        MouseHeld = -1
        WHILE _MOUSEBUTTON(1)
            _LIMIT 500
            SEND_PING
            mb = _MOUSEINPUT
            my = _MOUSEY
            mx = _MOUSEX
        WEND
        MouseHeld = 0
        mb = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click: RETURN

        IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
            'Click on variable lines
            IF DoubleClick THEN
                ContextualMenuLineRef = i
                GOSUB EditVariableRoutine
                DoubleClick = 0
                ShiftON = 0
                GOTO ToggleQuickWatch 'A double-click must undo the last toggle (single-click before double-click)
            ELSE
                'Toggle variable in source panel (QUICK WATCH)
                IF _KEYDOWN(100304) OR _KEYDOWN(100303) THEN ShiftON = -1 ELSE ShiftON = 0
                ToggleQuickWatch:
                IF ShiftON = 0 THEN
                    IF ASC(SELECTED_VARIABLES, i) = 0 OR ASC(SELECTED_VARIABLES, i) = 2 THEN
                        IF ASC(SELECTED_VARIABLES, i) = 0 THEN TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES + 1
                        ASC(SELECTED_VARIABLES, i) = 1
                        LastToggledON = i
                    ELSE
                        ASC(SELECTED_VARIABLES, i) = 0
                        TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES - 1
                    END IF
                ELSE
                    IF LastToggledON = 0 THEN ShiftON = 0: GOTO ToggleQuickWatch
                    IF i > LastToggledON THEN ToggleStep = -1 ELSE ToggleStep = 1
                    FOR ShiftToggle = i TO LastToggledON STEP ToggleStep
                        IF ASC(SELECTED_VARIABLES, ShiftToggle) = 0 OR ASC(SELECTED_VARIABLES, ShiftToggle) = 2 THEN
                            IF ASC(SELECTED_VARIABLES, ShiftToggle) = 0 THEN TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES + 1
                            ASC(SELECTED_VARIABLES, ShiftToggle) = 1
                        END IF
                    NEXT
                END IF
            END IF
        END IF
    END IF

    'Turn on contextual options if right mouse click and while in step mode.
    IF mb2 THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(2): _LIMIT 500: SEND_PING: mb2 = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb2 = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click

        IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
            'Set contextual menu coordinates relative to this item
            ShowPopupWatchpoint = 0
            ContextualMenuYRef = y
            ContextualMenuLineRef = i
            ContextualMenu.printY = printY
            ContextualMenu.FilteredList$ = FilteredList$

            MenuSetup$ = "": MenuID$ = ""
            MenuSetup$ = MenuSetup$ + IIFSTR$(FIRSTEXECUTION, "R&un", "Contin&ue") + CHR$(LF): MenuID$ = MenuID$ + MKI$(7)
            MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
            IF ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1 THEN
                MenuSetup$ = MenuSetup$ + "&Edit watchpoint" + CHR$(LF): MenuID$ = MenuID$ + MKI$(2)
                MenuSetup$ = MenuSetup$ + "&Clear watchpoint for '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "'" + CHR$(LF): MenuID$ = MenuID$ + MKI$(4)
            ELSE
                MenuSetup$ = MenuSetup$ + "S&et watchpoint" + CHR$(LF): MenuID$ = MenuID$ + MKI$(1)
            END IF
            IF TOTALWATCHPOINTS > 0 THEN
                MenuSetup$ = MenuSetup$ + "Clear all &watchpoints" + CHR$(LF): MenuID$ = MenuID$ + MKI$(5)
            END IF
            MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
            IF ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 1 THEN
                MenuSetup$ = MenuSetup$ + "Remove variable from &QUICK WATCH panel" + CHR$(LF): MenuID$ = MenuID$ + MKI$(8)
            ELSE
                MenuSetup$ = MenuSetup$ + "Add variable to &QUICK WATCH panel" + CHR$(LF): MenuID$ = MenuID$ + MKI$(8)
            END IF
            IF TOTAL_SELECTEDVARIABLES > 0 THEN
                MenuSetup$ = MenuSetup$ + "Clear all variables from &QUICK WATCH" + CHR$(LF): MenuID$ = MenuID$ + MKI$(9)
            END IF
            IF TRIM$(UCASE$(VARIABLES(ContextualMenuLineRef).SCOPE)) <> UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)) AND TRIM$(VARIABLES(ContextualMenuLineRef).SCOPE) <> "SHARED" THEN
                'Can't edit variable outside scope.
            ELSE
                MenuSetup$ = MenuSetup$ + "-" + CHR$(LF): MenuID$ = MenuID$ + MKI$(0)
                MenuSetup$ = MenuSetup$ + "Edit &value of '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "'" + CHR$(LF): MenuID$ = MenuID$ + MKI$(3)
            END IF

            Choice = SHOWMENU(MenuSetup$, MenuID$, mx, my)
            MenuWasInvoked = -1
            SELECT CASE Choice
                CASE 5
                    'Clear all watchpoints
                    WATCHPOINTLIST = STRING$(CLIENT.TOTALVARIABLES, 0)
                    TOTALWATCHPOINTS = 0
                    FOR clear.WP = 1 TO CLIENT.TOTALVARIABLES
                        WATCHPOINT(clear.WP).EXPRESSION = ""
                    NEXT
                CASE 9
                    'Clear all variables from QUICK WATCH
                    SELECTED_VARIABLES = STRING$(CLIENT.TOTALVARIABLES, 0)
                    TOTAL_SELECTEDVARIABLES = 0
                CASE 8
                    'Toggle variable in QUICK WATCH
                    IF ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 0 OR ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 2 THEN
                        IF ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 0 THEN TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES + 1
                        ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 1
                    ELSE
                        ASC(SELECTED_VARIABLES, ContextualMenuLineRef) = 0
                        TOTAL_SELECTEDVARIABLES = TOTAL_SELECTEDVARIABLES - 1
                    END IF
                CASE 4
                    'Clear watchpoint
                    ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 0
                    TOTALWATCHPOINTS = TOTALWATCHPOINTS - 1
                    WATCHPOINT(ContextualMenuLineRef).EXPRESSION = ""
                CASE 1, 2
                    'Create a watchpoint
                    GET #FILE, DATABLOCK, VARIABLE_DATA()
                    Message$ = "Run until '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "' (" + TRIM$(VARIABLES(ContextualMenuLineRef).DATATYPE) + ")" + CHR$(LF)
                    Message$ = Message$ + "meets the following condition: (you can use =, <>, >, >=, <, <=)"
                    InitialValue$ = "=" + VARIABLE_DATA(ContextualMenuLineRef).VALUE
                    InitialSelection = 1
                    IF LEN(TRIM$(WATCHPOINT(ContextualMenuLineRef).EXPRESSION)) > 0 THEN InitialValue$ = TRIM$(WATCHPOINT(ContextualMenuLineRef).EXPRESSION): InitialSelection = -1
                    MESSAGEBOX_RESULT = INPUTBOX("Set a watchpoint", Message$, InitialValue$, NewValue$, InitialSelection, -1)
                    IF MESSAGEBOX_RESULT = 2 THEN GOTO WatchPointDone
                    IF LEN(NewValue$) < 2 THEN
                        IF ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1 THEN TOTALWATCHPOINTS = TOTALWATCHPOINTS - 1
                        ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 0
                        WATCHPOINT(ContextualMenuLineRef).EXPRESSION = ""
                    ELSE
                        StartWatchPointEval:
                        op1$ = MID$(NewValue$, 1, 1)
                        op2$ = MID$(NewValue$, 2, 1)
                        SELECT CASE op1$
                            CASE "="
                                IF op2$ = "<" OR op2$ = ">" THEN
                                    MID$(NewValue$, 1, 2) = op2$ + "="
                                    GOTO StartWatchPointEval
                                END IF
                                IF ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 0 THEN TOTALWATCHPOINTS = TOTALWATCHPOINTS + 1
                                ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1
                                WATCHPOINT(ContextualMenuLineRef).EXPRESSION = NewValue$
                            CASE ">"
                                IF op2$ = "<" OR op2$ = ">" THEN
                                    GOTO WatchpointInvalidExpression
                                END IF
                                IF ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 0 THEN TOTALWATCHPOINTS = TOTALWATCHPOINTS + 1
                                ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1
                                WATCHPOINT(ContextualMenuLineRef).EXPRESSION = NewValue$
                            CASE "<"
                                IF ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 0 THEN TOTALWATCHPOINTS = TOTALWATCHPOINTS + 1
                                ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1
                                WATCHPOINT(ContextualMenuLineRef).EXPRESSION = NewValue$
                            CASE ELSE
                                GOTO WatchpointInvalidExpression
                        END SELECT
                    END IF
                    GOTO WatchPointDone

                    WatchpointInvalidExpression:
                    MESSAGEBOX_RESULT = MESSAGEBOX("Set a watchpoint", "Invalid expression.", MKI$(OK_ONLY), 1, -1)

                    WatchPointDone:
                CASE 3
                    'Edit
                    EditVariableRoutine:
                    IF (TRIM$(UCASE$(VARIABLES(ContextualMenuLineRef).SCOPE)) <> UCASE$(GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2))) AND TRIM$(VARIABLES(ContextualMenuLineRef).SCOPE) <> "SHARED" THEN
                        Message$ = ""
                        Message$ = Message$ + "Cannot edit '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "' (" + TRIM$(VARIABLES(ContextualMenuLineRef).DATATYPE) + ") until program execution is" + CHR$(LF)
                        Message$ = Message$ + "inside " + TRIM$(VARIABLES(ContextualMenuLineRef).SCOPE) + "."
                        MESSAGEBOX_RESULT = MESSAGEBOX("Out of scope", Message$, MKI$(OK_ONLY), 1, -1)
                    ELSE
                        DataType$ = VARIABLES(ContextualMenuLineRef).DATATYPE
                        Message$ = "New value for '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "' (" + TRIM$(VARIABLES(ContextualMenuLineRef).DATATYPE) + ")"
                        MESSAGEBOX_RESULT = INPUTBOX("Edit variable", Message$, VARIABLE_DATA(ContextualMenuLineRef).VALUE, NewValue$, -1, -1)
                        IF MESSAGEBOX_RESULT = 1 THEN
                            'Send to the client:
                            '1- Variable index to change;
                            '2- MKL$(data size);
                            '3- Actual data.
                            EXCHANGEDATASIZE$4 = MKL$(ContextualMenuLineRef)
                            PUT #FILE, EXCHANGEBLOCK, EXCHANGEDATASIZE$4
                            CONVERSIONERRORRAISED = 0
                            ON ERROR GOTO DataConversionERROR
                            SELECT CASE UCASE$(TRIM$(DataType$))
                                CASE "_BIT"
                                    EXCHANGEDATA = _MK$(_BIT, VAL(NewValue$))
                                CASE "_UNSIGNED _BIT"
                                    EXCHANGEDATA = _MK$(_UNSIGNED _BIT, VAL(NewValue$))
                                CASE "_BYTE"
                                    EXCHANGEDATA = _MK$(_BYTE, VAL(NewValue$))
                                CASE "_UNSIGNED _BUTE"
                                    EXCHANGEDATA = _MK$(_BIT, VAL(NewValue$))
                                CASE "INTEGER"
                                    EXCHANGEDATA = _MK$(INTEGER, VAL(NewValue$))
                                CASE "_UNSIGNED INTEGER"
                                    EXCHANGEDATA = _MK$(_UNSIGNED INTEGER, VAL(NewValue$))
                                CASE "LONG"
                                    EXCHANGEDATA = _MK$(LONG, VAL(NewValue$))
                                CASE "_UNSIGNED LONG"
                                    EXCHANGEDATA = _MK$(_UNSIGNED LONG, VAL(NewValue$))
                                CASE "_INTEGER64"
                                    EXCHANGEDATA = _MK$(_INTEGER64, VAL(NewValue$))
                                CASE "_UNSIGNED _INTEGER64"
                                    EXCHANGEDATA = _MK$(_UNSIGNED _INTEGER64, VAL(NewValue$))
                                CASE "SINGLE"
                                    EXCHANGEDATA = _MK$(SINGLE, VAL(NewValue$))
                                CASE "DOUBLE"
                                    EXCHANGEDATA = _MK$(DOUBLE, VAL(NewValue$))
                                CASE "_FLOAT"
                                    EXCHANGEDATA = _MK$(_FLOAT, VAL(NewValue$))
                                CASE "STRING"
                                    EXCHANGEDATA = NewValue$
                            END SELECT
                            ON ERROR GOTO 0
                            IF CONVERSIONERRORRAISED THEN
                                Message$ = ""
                                Message$ = Message$ + "Value could not be set (variable type is " + TRIM$(DataType$) + ")." + CHR$(LF)
                                MESSAGEBOX_RESULT = MESSAGEBOX("Invalid input", Message$, MKI$(OK_ONLY), 1, -1)
                            ELSE
                                EXCHANGEDATASIZE$4 = MKL$(LEN(EXCHANGEDATA))
                                PUT #FILE, , EXCHANGEDATASIZE$4
                                PUT #FILE, , EXCHANGEDATA
                                BREAKPOINT.ACTION = SETVAR
                                PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
                            END IF
                        END IF
                        IF DoubleClick THEN RETURN
                    END IF
                CASE 7
                    Clicked = -1
                    GOSUB RunButton_Click
            END SELECT
        END IF
    END IF
    RETURN

    CheckButtons:
    IF ShowContextualMenu THEN
        Clicked = 0
        MenuHoverHighlight:
        IF (mx >= ContextualMenu.X) AND (mx <= ContextualMenu.X + ContextualMenu.W) THEN
            IF (my >= ContextualMenu.Y + 4) AND (my <= ContextualMenu.Y + 4 + _FONTHEIGHT) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4 + _FONTHEIGHT)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            END IF
        END IF

        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4), " Set/Edit a watchpoint "
        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4 + _FONTHEIGHT), " Edit variable value   "
        IF MouseHeld THEN RETURN
    ELSE
        IF my > _FONTHEIGHT THEN _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$: RETURN
        'Hover highlight:
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                LINE (Buttons(cb).X - 3, 3)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
            END IF
        NEXT cb

        _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$

        IF mb THEN
            FOR cb = 1 TO TotalButtons
                IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                    WHILE _MOUSEBUTTON(1): _LIMIT 500: SEND_PING: mb = _MOUSEINPUT: WEND
                    mb = 0: mx = _MOUSEX: my = _MOUSEY
                    'Check if the user moved the mouse out of the button before releasing it (=cancel)
                    IF my > _FONTHEIGHT THEN RETURN
                    IF (mx < Buttons(cb).X) OR (mx > Buttons(cb).X + Buttons(cb).W) THEN RETURN
                    Clicked = -1
                    SELECT CASE Buttons(cb).ID
                        CASE 1: GOSUB RunButton_Click
                        CASE 2: GOSUB WindowButton_Click
                        CASE 3: GOSUB StepButton_Click
                        CASE 4: GOSUB ToggleButton_Click
                        CASE 5: GOSUB ClearButton_CLICK
                        CASE 6: GOSUB ExitButton_Click
                        CASE 7: VARIABLE_HIGHLIGHT = NOT VARIABLE_HIGHLIGHT
                        CASE ELSE: SYSTEM_BEEP 0
                    END SELECT
                END IF
            NEXT cb
        END IF
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
SUB INTERACTIVE_MODE (AddedList$, TotalSelected)
    'Allows user to select which of the found variables will be watched.
    'Shows a UI similar to monitor mode, with extra commands to filter/add.

    DIM SB_Ratio AS SINGLE
    DIM ListEnd_Label AS STRING

    TotalButtons = 5
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    AddedList$ = STRING$(TOTALVARIABLES, 0) 'Start interactive mode with all variables unselected
    TotalSelected = 0

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS

    Filter$ = ""
    searchIn = VARIABLENAMES
    SB_ThumbY = 0
    grabbedY = -1
    ListEnd_Label = "(end of list)"
    _KEYCLEAR

    LONGESTVARNAME = 1
    LONGESTSCOPESPEC = 1
    FOR i = 1 TO TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > LONGESTVARNAME THEN LONGESTVARNAME = LEN(TRIM$(VARIABLES(i).NAME))
        IF LEN(TRIM$(VARIABLES(i).SCOPE)) > LONGESTSCOPESPEC THEN LONGESTSCOPESPEC = LEN(TRIM$(VARIABLES(i).SCOPE))
    NEXT i

    LEAVE_INTERACTIVE_MODE = 0
    DO: _LIMIT 500
        GOSUB ProcessInput
        GOSUB UpdateList

        IF _EXIT THEN
            CLOSE
            KILL NEWFILENAME$
            SYSTEM
        END IF
    LOOP UNTIL LEAVE_INTERACTIVE_MODE

    COLOR _RGB32(0, 0, 0), _RGB32(230, 230, 230)

    EXIT SUB
    ProcessInput:
    k = _KEYHIT: modKey = k
    IF modKey = 100303 OR modKey = 100304 THEN shiftDown = -1
    IF modKey = -100303 OR modKey = -100304 THEN shiftDown = 0
    IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
    IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0

    DO
        y = y + (_MOUSEWHEEL * (LIST_AREA / 3))
        mx = _MOUSEX
        my = _MOUSEY
        mb = _MOUSEBUTTON(1)
    LOOP WHILE _MOUSEINPUT

    SELECT EVERYCASE k
        CASE 86, 118 'V
            IF ctrlDown = -1 THEN
                IF LEN(_CLIPBOARD$) THEN Filter$ = Filter$ + _CLIPBOARD$
                k = 0
            END IF
        CASE 32 TO 126 'Printable ASCII characters
            Filter$ = Filter$ + CHR$(k)
        CASE 8 'Backspace
            IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
        CASE 9, 25 'TAB alternates between what is filtered (VARIABLENAMES, DATATYPES)
            IF searchIn = SCOPE THEN Filter$ = ""
            SELECT CASE searchIn
                CASE VARIABLENAMES: IF shiftDown = 0 THEN searchIn = SCOPE ELSE searchIn = DATATYPES
                CASE SCOPE: IF shiftDown = 0 THEN searchIn = DATATYPES ELSE searchIn = VARIABLENAMES
                CASE DATATYPES: IF shiftDown = 0 THEN searchIn = VARIABLENAMES ELSE searchIn = SCOPE
            END SELECT
        CASE 27 'ESC clears the current search filter or exits interactive mode
            CancelButton_Click:
            IF LEN(Filter$) THEN
                Filter$ = ""
            ELSE
                AddedList$ = CHR$(3)
                COLOR _RGB32(0, 0, 0), _RGB32(230, 230, 230)
                EXIT SUB
            END IF
        CASE 18432 'Up
            ArrowStep! = (LIST_AREA / 10)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - ArrowStep!
        CASE 20480 'Down
            ArrowStep! = (LIST_AREA / 10)
            IF ArrowStep! < _FONTHEIGHT THEN ArrowStep! = _FONTHEIGHT
            IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + ArrowStep!
        CASE 15360 'F2
            SelectButton_Click:
            IF LEN(Filter$) = 0 THEN
                AddedList$ = STRING$(TOTALVARIABLES, 1)
                TotalSelected = TOTALVARIABLES
            ELSE
                FOR i = 1 TO LEN(FilteredList$) / 4
                    item = CVL(MID$(FilteredList$, i * 4 - 3, 4))
                    IF ASC(AddedList$, item) = 0 THEN
                        ASC(AddedList$, item) = 1: TotalSelected = TotalSelected + 1
                    END IF
                NEXT i
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 15616 'F3
            ClearButton_Click:
            IF LEN(Filter$) = 0 THEN
                AddedList$ = STRING$(TOTALVARIABLES, 0)
                TotalSelected = 0
            ELSE
                FOR i = 1 TO LEN(FilteredList$) / 4
                    item = CVL(MID$(FilteredList$, i * 4 - 3, 4))
                    IF ASC(AddedList$, item) = 1 THEN
                        ASC(AddedList$, item) = 0: TotalSelected = TotalSelected - 1
                    END IF
                NEXT i
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16128 'F5
            SaveButton_Click:
            LEAVE_INTERACTIVE_MODE = -1
            IF Clicked THEN Clicked = 0: RETURN
    END SELECT

    'Scrollbar check:
    IF PAGE_HEIGHT > LIST_AREA THEN
        IF mb THEN
            IF mx > _WIDTH(MAINSCREEN) - 30 AND mx < _WIDTH(MAINSCREEN) THEN
                'Clicked inside the scroll bar. Check if click was on the thumb:
                IF my > SCREEN_TOPBAR + SB_ThumbY + 24 AND my < SCREEN_TOPBAR + SB_ThumbY + 24 + SB_ThumbH THEN
                    'Clicked on the thumb:
                    grabbedY = my: starty = y
                    DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
                    DO WHILE _MOUSEBUTTON(1): _LIMIT 500
                        WHILE _MOUSEINPUT: WEND
                        my = _MOUSEY
                        y = starty + ((my - grabbedY) / SB_Ratio)

                        CHECK_SCREEN_LIMITS y
                        IF prevY <> y THEN
                            DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my: prevY = y
                            GOSUB UpdateList
                        END IF
                        'SEND_PING
                        _DISPLAY
                    LOOP
                    grabbedY = -1
                ELSEIF my > SCREEN_TOPBAR AND my <= SCREEN_TOPBAR + 20 THEN
                    'Up arrow
                    IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - (LIST_AREA / 10)
                    _DELAY .1
                ELSEIF my > SCREEN_HEIGHT - 21 THEN
                    'Down arrow
                    IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + (LIST_AREA / 10)
                    _DELAY .1
                ELSE
                    'Clicked above or below the thumb:
                    IF my < SCREEN_TOPBAR + 25 + SB_ThumbY AND my > SCREEN_TOPBAR + 21 THEN
                        y = y - ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    ELSEIF my > SCREEN_TOPBAR + 25 + SB_ThumbY + SB_ThumbH AND my < SCREEN_HEIGHT - 21 THEN
                        y = y + ((LIST_AREA / 6) / SB_Ratio)
                        _DELAY .1
                    END IF
                END IF
            END IF
        END IF
    END IF
    RETURN

    UpdateList:
    CHECK_RESIZE 0, 0
    cursorBlink% = cursorBlink% + 1
    IF cursorBlink% > 50 THEN cursorBlink% = 0
    'Build a filtered list, if a filter is active:
    i = 0: FilteredList$ = ""
    PAGE_HEIGHT = _FONTHEIGHT * (TOTALVARIABLES + 3)
    IF LEN(Filter$) > 0 THEN
        DO
            i = i + 1
            IF i > TOTALVARIABLES THEN EXIT DO
            Found = 0
            SELECT CASE searchIn
                CASE VARIABLENAMES: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).NAME), UCASE$(Filter$))
                CASE DATATYPES: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).DATATYPE), UCASE$(Filter$))
                CASE SCOPE: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).SCOPE), UCASE$(Filter$))
            END SELECT
            IF Found THEN
                FilteredList$ = FilteredList$ + MKL$(i)
            END IF
        LOOP
        IF LEN(FilteredList$) > 0 THEN PAGE_HEIGHT = _FONTHEIGHT * ((LEN(FilteredList$) / 4) + 3)
    END IF

    CHECK_SCREEN_LIMITS y

    'Place a light gray rectangle under the column that can currently be filtered:
    SELECT CASE searchIn
        CASE DATATYPES
            columnHighlightX = _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 7))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 28))
            columnHighlightW = _PRINTWIDTH(SPACE$(LONGESTVARNAME)) + 8
        CASE SCOPE
            columnHighlightX = _PRINTWIDTH(SPACE$(6))
            columnHighlightW = _PRINTWIDTH(SPACE$(LONGESTSCOPESPEC + 1))
    END SELECT
    columnHighlightY = 51

    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        columnHightlighH = (LEN(FilteredList$) / 4 * _FONTHEIGHT) + _FONTHEIGHT
    ELSEIF LEN(Filter$) > 0 AND LEN(FilteredList$) = 0 THEN
        columnHightlighH = 0
    ELSE
        columnHightlighH = (TOTALVARIABLES * _FONTHEIGHT) + _FONTHEIGHT
    END IF
    CLS , _RGB32(255, 255, 255)
    LINE (columnHighlightX, columnHighlightY)-STEP(columnHighlightW, columnHightlighH), _RGB32(230, 230, 230), BF

    'Get mouse coordinates:
    DO
    LOOP WHILE _MOUSEINPUT
    mx = _MOUSEX: my = _MOUSEY: mb = _MOUSEBUTTON(1)

    'Print list items to the screen:
    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        FOR ii = ((y \ _FONTHEIGHT) + 1) TO LEN(FilteredList$) / 4
            i = CVL(MID$(FilteredList$, ii * 4 - 3, 4))
            printY = ((3 + ii) * _FONTHEIGHT) - y
            IF printY > SCREEN_HEIGHT THEN EXIT FOR
            GOSUB ColorizeSelection
            IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + LEFT$(VARIABLES(i).SCOPE, LONGESTSCOPESPEC) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, LONGESTVARNAME)
            _PRINTSTRING (5, printY), v$
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO TOTALVARIABLES
            printY = ((3 + i) * _FONTHEIGHT) - y
            IF printY > SCREEN_HEIGHT THEN EXIT FOR
            GOSUB ColorizeSelection
            IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + LEFT$(VARIABLES(i).SCOPE, LONGESTSCOPESPEC) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, LONGESTVARNAME)
            _PRINTSTRING (5, printY), v$
        NEXT i
    END IF

    IF LEN(Filter$) AND LEN(FilteredList$) = 0 THEN 'A filter is on, but nothing was found
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT), "Not found."
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to clear)"
    END IF

    'Top bar:
    '  INTERACTIVE MODE: <F2 = Select all> <F3 = Clear all> <F5 = Save and continue> <ESC = Cancel>
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), 50), _RGB32(179, 255, 255), BF
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    ModeTitle$ = "INTERACTIVE MODE: "
    _PRINTSTRING (5, 3), ModeTitle$
    COLOR _RGB32(255, 255, 255)
    _PRINTSTRING (4, 2), ModeTitle$
    COLOR _RGB32(0, 0, 0)
    totalinfo$ = NOPATH$(FILENAME$) + " - Variables found: " + TRIM$(STR$(TOTALVARIABLES)) + "   Selected: " + TRIM$(STR$(TotalSelected))
    _PRINTSTRING (5, (_FONTHEIGHT + 3)), totalinfo$
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), IIFSTR$(LEN(Filter$), "Filter: " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), ""), "Filter: " + IIFSTR$(cursorBlink% > 25, CHR$(179), ""))


    'Top buttons:
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F2=Select" + IIFSTR$(LEN(Filter$), " all filtered>", " all>"): b = b + 1
    IF TotalSelected > 0 THEN
        Buttons(b).ID = 2: Buttons(b).CAPTION = "<F3=Clear" + IIFSTR$(LEN(Filter$), " all filtered>", " all>"): b = b + 1
    END IF
    Buttons(b).ID = 3: Buttons(b).CAPTION = IIFSTR$(TotalSelected > 0, "<F5=Save and Continue>", "<F5=Continue>"): b = b + 1
    Buttons(b).ID = 4: Buttons(b).CAPTION = IIFSTR$(LEN(Filter$) > 0, "<ESC=Clear filter>", "<ESC=Exit>"): b = b + 1

    FOR cb = b TO TotalButtons
        Buttons(cb).CAPTION = ""
    NEXT cb

    ButtonLine$ = ""
    FOR cb = 1 TO TotalButtons
        c$ = TRIM$(Buttons(cb).CAPTION)
        ButtonLine$ = ButtonLine$ + IIFSTR$(LEN(c$), c$ + " ", "")
    NEXT cb

    FOR cb = 1 TO TotalButtons
        Buttons(cb).X = INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION)) * _FONTWIDTH + _PRINTWIDTH(ModeTitle$)
        Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
    NEXT cb

    GOSUB CheckButtons
    IF LEAVE_INTERACTIVE_MODE THEN RETURN

    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$
    FOR i = 1 TO LEN(ButtonLine$)
        IF (ASC(ButtonLine$, i) <> 60) AND (ASC(ButtonLine$, i) <> 62) THEN
            ASC(ButtonLine$, i) = 32
        END IF
    NEXT i
    COLOR _RGB32(255, 255, 0)
    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 2), ButtonLine$
    COLOR _RGB32(0, 0, 0)


    IF PAGE_HEIGHT > LIST_AREA THEN
        IF LEN(Filter$) AND LEN(FilteredList$) > 0 THEN
            _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
        ELSEIF LEN(Filter$) = 0 THEN
            _PRINTSTRING (5, ((5 + TOTALVARIABLES) * _FONTHEIGHT) - y), ListEnd_Label
        END IF
        DISPLAYSCROLLBAR y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio, mx, my
    ELSE
        'End of list message:
        IF LEN(Filter$) AND LEN(FilteredList$) > 0 THEN
            _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
        ELSEIF LEN(Filter$) = 0 THEN
            _PRINTSTRING (5, PAGE_HEIGHT + (_FONTHEIGHT * 2) - y), ListEnd_Label
        END IF
    END IF

    _DISPLAY
    RETURN

    ColorizeSelection:
    'Indicate that a line has been selected with a light green bg
    IF ASC(AddedList$, i) = 0 THEN RETURN
    LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(0, 200, 0, 100), BF
    RETURN

    DetectClick:
    'Place a hover indicator over this item:
    LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 200), BF

    'Select/Clear the item if a mouse click was detected.
    IF mb THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(1): mb = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb = 0

        IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
            IF ASC(AddedList$, i) = 1 THEN
                ASC(AddedList$, i) = 0
                TotalSelected = TotalSelected - 1
            ELSE
                ASC(AddedList$, i) = 1
                TotalSelected = TotalSelected + 1
            END IF
        END IF
    END IF
    RETURN

    CheckButtons:
    Clicked = 0
    IF my > _FONTHEIGHT THEN RETURN
    'Hover highlight:
    FOR cb = 1 TO TotalButtons
        IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
            LINE (Buttons(cb).X - 3, 3)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
        END IF
    NEXT cb

    IF mb THEN
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                WHILE _MOUSEBUTTON(1): _LIMIT 500: mb = _MOUSEINPUT: WEND
                mb = 0: mx = _MOUSEX: my = _MOUSEY
                'Check if the user moved the mouse out of the button before releasing it (=cancel)
                IF my > _FONTHEIGHT THEN RETURN
                IF (mx < Buttons(cb).X) OR (mx > Buttons(cb).X + Buttons(cb).W) THEN RETURN
                SELECT CASE Buttons(cb).ID
                    CASE 1: GOSUB SelectButton_Click
                    CASE 2: GOSUB ClearButton_Click
                    CASE 3: GOSUB SaveButton_Click
                    CASE 4: GOSUB CancelButton_Click
                    CASE ELSE: SYSTEM_BEEP 0
                END SELECT
            END IF
        NEXT cb
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
FUNCTION OpenInclude (f$, CodeText() AS STRING, Lines&)
    'OpenInclude adapted from codeguy's routines found in
    'http://www.qb64.net/forum/index.php?topic=1565.msg17025#msg17025
    DIM insc1%, insc2%, tabs%
    DIM FoundInclude AS _BYTE
    DIM InclResult AS INTEGER

    IF _FILEEXISTS(f$) THEN
        c% = FREEFILE
        OPEN f$ FOR BINARY AS c%
        DO
            IF EOF(c%) THEN
                EXIT DO
            ELSE
                LINE INPUT #c%, readx$
                Lines& = Lines& + 1
                GOSUB AddStringToArray
                readx$ = TRIM$(readx$)
                CommentStart = LEN(STRIPCOMMENTS$(readx$))
                IF CommentStart = 0 THEN CommentStart = 1
                insinc% = INSTR(CommentStart, UCASE$(readx$), "$INCLUDE:")
                IF insinc% > 0 THEN
                    IF MID$(readx$, CommentStart, 1) = "'" THEN
                        CommentStart = CommentStart + 1
                    ELSEIF MID$(UCASE$(readx$), CommentStart, 4) = "REM " THEN
                        CommentStart = CommentStart + 3
                    END IF
                    TextBeforeMetaCommand$ = MID$(readx$, CommentStart, insinc% - CommentStart)
                    IF LEN(TRIM$(TextBeforeMetaCommand$)) > 0 THEN GOTO InvalidInclude
                    FoundInclude = -1
                    insc1% = INSTR(insinc%, readx$, "'")
                    insc2% = INSTR(insc1% + 1, readx$, "'")
                    IncludedFile$ = MID$(readx$, insc1% + 1, insc2% - insc1% - 1)
                    CodeText(Lines&) = "'*INCLUDE file merged: '" + IncludedFile$ + "'"
                    $IF WIN THEN
                        IF LEFT$(IncludedFile$, 1) = "\" OR INSTR(IncludedFile$, ":") > 0 THEN
                            'Do nothing; it's an absolute path.
                        ELSE
                            tIncludedFile$ = PATHONLY$(f$) + IncludedFile$
                            IF _FILEEXISTS(tIncludedFile$) = 0 THEN
                                'If file not found with path relative to the current source,
                                'attempt to find it relative to QB64's folder
                                IncludedFile$ = _CWD$ + PATHSEP$ + IncludedFile$
                            ELSE
                                IncludedFile$ = tIncludedFile$
                            END IF
                        END IF
                    $ELSE
                        IF LEFT$(IncludedFile$, 1) = "/" THEN
                        'Do nothing; it's an absolute path.
                        ELSE
                        IncludedFile$ = PATHONLY$(f$) + IncludedFile$
                        END IF
                    $END IF
                    InclResult = OpenInclude(IncludedFile$, CodeText(), Lines&)
                    IF InclResult = MISSINGFILE THEN f$ = IncludedFile$: OpenInclude = MISSINGFILE: EXIT FUNCTION
                END IF
                InvalidInclude:
            END IF
        LOOP
        CLOSE c%
    ELSE
        OpenInclude = MISSINGFILE: EXIT FUNCTION
    END IF

    IF FoundInclude THEN OpenInclude = MERGESUCCESSFUL ELSE OpenInclude = NOINCLUDES

    EXIT FUNCTION
    AddStringToArray:
    IF Lines& > UBOUND(CodeText) THEN REDIM _PRESERVE CodeText(1 TO UBOUND(CodeText) + 32)
    tabs% = 0
    DO WHILE LEFT$(readx$, 1) = CHR$(9)
        readx$ = MID$(readx$, 2)
        tabs% = tabs% + 1
    LOOP
    readx$ = SPACE$(tabs% * 4) + readx$
    CodeText(Lines&) = readx$
    RETURN
END FUNCTION

'------------------------------------------------------------------------------
SUB PROCESSFILE
    'Parses a .BAS file and reads all compatible variables
    'in order to generate a compatible vWATCH64 client.

    DIM CHECKSUM AS STRING * 8
    DIM CheckingOff AS _BYTE
    DIM DeclaringLibrary AS _BYTE
    DIM DefaultTypeUsed AS _BYTE
    DIM DefiningType AS _BYTE
    DIM FoundType AS STRING
    DIM InBetweenSubs AS _BYTE
    DIM IsArray AS _BYTE
    DIM LocalVariable AS _BYTE
    DIM MULTILINE AS _BYTE
    DIM MULTILINE_DIM AS _BYTE
    DIM MainModule AS _BYTE
    DIM MainModuleEND AS LONG
    DIM NextVar$
    DIM OutputFile AS INTEGER
    DIM PrecompilerBlock AS _BYTE
    DIM ProcessLine AS LONG
    DIM ProcessStepDescription AS STRING
    DIM SET_OPTION_EXPLICIT AS INTEGER
    DIM SourceLine AS STRING
    DIM StatusMessage AS STRING
    DIM ThisKeyword AS STRING
    DIM ThisLineHasBPControl AS LONG
    DIM TotalExpandedWithUDT AS INTEGER
    DIM TotalKeywords AS INTEGER
    DIM TotalLocalVariables AS INTEGER
    DIM TotalNextLineData AS LONG
    DIM TotalSourceLines AS LONG
    DIM TotalSubFunc AS LONG
    DIM TotalUDTs AS INTEGER
    DIM TotalUDTsAdded AS INTEGER
    DIM bkpSourceLine$
    DIM caseBkpNextVar$
    DIM caseBkpSourceLine AS STRING
    REDIM ExpandedWithUDT(1) AS VARIABLESTYPE
    REDIM KeywordList(1) AS STRING
    REDIM LOCALSHAREDADDED(1) AS STRING
    REDIM LOCALVARIABLES(1) AS VARIABLESTYPE
    REDIM OutputLines(1) AS STRING
    REDIM SetNextLineData(1) AS STRING
    REDIM UDT(1) AS UDTTYPE, UDT_ADDED(1) AS VARIABLESTYPE

    RESTORE DataTypeKeywordDATA
    'Populate KeywordList() with DATA TYPES:
    DO
        READ ThisKeyword
        IF ThisKeyword = "**END**" THEN
            INTERNALKEYWORDS = TotalKeywords
            EXIT DO
        END IF
        GOSUB AddThisKeyword
    LOOP

    Q$ = CHR$(34)

    IF _FILEEXISTS(FILENAME$) THEN
        TempFileCheck% = FREEFILE
        OPEN FILENAME$ FOR BINARY AS TempFileCheck%
        TempFileContents$ = SPACE$(LOF(TempFileCheck%))
        GET #TempFileCheck%, 1, TempFileContents$
        FoundEvidence1 = INSTR(TempFileContents$, "SUB VWATCH64_STOPTIMERS ALIAS stop_timers")
        IF FoundEvidence1 > 0 THEN FoundEvidence1 = 1
        FoundEvidence2 = INSTR(TempFileContents$, "CONST vwatch64_ID = " + Q$ + "vWATCH64" + Q$)
        IF FoundEvidence2 > 0 THEN FoundEvidence2 = 1
        FoundEvidence = FoundEvidence1 + FoundEvidence2
        TempFileContents$ = ""
        CLOSE TempFileCheck%
        IF FoundEvidence = 2 THEN
            Message$ = "This file is already a vWATCH64 client and" + CHR$(LF)
            Message$ = Message$ + "cannot be processed again."
            MessageSetup$ = MKI$(OK_ONLY)
            MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MessageSetup$, 1, 0)
            EXIT SUB
        END IF
    END IF

    'IF LEN(TRIM$(NEWFILENAME$)) = 0 THEN
    '    i = -1
    '    InputNewFileName:
    '    DO: _LIMIT 30
    '        i = i + 1
    '        IF UCASE$(RIGHT$(FILENAME$, 4)) = ".BAS" THEN
    '            NEWFILENAME$ = LEFT$(FILENAME$, LEN(FILENAME$) - 4) + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
    '        ELSE
    '            NEWFILENAME$ = FILENAME$ + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
    '        END IF
    '        IF _FILEEXISTS(NEWFILENAME$) = 0 THEN EXIT DO
    '    LOOP
    'ELSE
    '    i = -1
    '    tempFilename$ = NEWFILENAME$
    '    DO: _LIMIT 30
    '        i = i + 1
    '        IF UCASE$(RIGHT$(tempFilename$, 4)) = ".BAS" THEN
    '            NEWFILENAME$ = LEFT$(tempFilename$, LEN(tempFilename$) - 4) + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
    '        ELSE
    '            NEWFILENAME$ = tempFilename$ + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
    '        END IF
    '        IF _FILEEXISTS(NEWFILENAME$) = 0 THEN EXIT DO
    '    LOOP
    'END IF
    InputNewFileName:
    IF UCASE$(RIGHT$(FILENAME$, 4)) = ".BAS" THEN
        NEWFILENAME$ = LEFT$(FILENAME$, LEN(FILENAME$) - 4) + ".vwatch.bas"
    ELSE
        NEWFILENAME$ = FILENAME$ + ".vwatch.bas"
    END IF

    TempPath$ = PATHONLY$(NEWFILENAME$)
    NEWFILENAME$ = NOPATH$(NEWFILENAME$)
    MESSAGEBOX_RESULT = INPUTBOX("Process .BAS", "Output file name:", NEWFILENAME$, NEWFILENAME$, -1, 0)
    IF MESSAGEBOX_RESULT = 2 THEN EXIT SUB

    NEWFILENAME$ = TempPath$ + NEWFILENAME$
    IF NEWFILENAME$ = FILENAME$ THEN
        Message$ = "Cannot overwrite an original source file."
        MessageSetup$ = MKI$(MB_CUSTOM) + "Enter new name" + CHR$(LF) + "Cancel"
        MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MessageSetup$, 1, 0)
        IF MESSAGEBOX_RESULT = 1 THEN i = 0: GOTO InputNewFileName ELSE EXIT SUB
    END IF

    IF _FILEEXISTS(NEWFILENAME$) THEN
        Message$ = "'" + NOPATH$(NEWFILENAME$) + "' already exists. Overwrite?"
        MessageSetup$ = MKI$(MB_CUSTOM) + "Yes" + CHR$(LF) + "No " + CHR$(LF) + "Cancel"
        MESSAGEBOX_RESULT = MESSAGEBOX("File already exists", Message$, MessageSetup$, 1, 0)
        IF MESSAGEBOX_RESULT = 2 THEN i = 0: GOTO InputNewFileName
        IF MESSAGEBOX_RESULT = 3 OR MESSAGEBOX_RESULT = -1 THEN EXIT SUB
    END IF

    'Processing can proceed.
    _AUTODISPLAY
    COLOR _RGB32(0, 0, 0), _RGB32(230, 230, 230)
    CLS
    ProcessStepDescription = "Checking $INCLUDE files...": VerboseMaxProgress = 100: VerboseProgress = 0: GOSUB AddVerboseOutputLine
    MergeResult = OpenInclude(FILENAME$, SOURCECODE(), TotalSourceLines)
    IF MergeResult = MISSINGFILE THEN
        Message$ = ""
        Message$ = Message$ + "One of the $INCLUDE files could not be found" + CHR$(LF)
        Message$ = Message$ + "('" + NOPATH$(FILENAME$) + "' on line" + STR$(TotalSourceLines) + ")."
        PCOPY 0, 1
        MESSAGEBOX_RESULT = MESSAGEBOX("Processing failed", Message$, MKI$(OK_ONLY), 1, 0)
        EXIT SUB
    ELSEIF MergeResult = MERGESUCCESSFUL THEN
        ProcessStepDescription = "Source file has $INCLUDE files; merge successful.": VerboseMaxProgress = 100: VerboseProgress = 100: GOSUB AddVerboseOutputLine
    END IF

    'Calculate checksum:
    ProcessStepDescription = "Loading file and all $INCLUDE modules..."
    VerboseMaxProgress = TotalSourceLines
    SOURCEFILE = ""
    UpdateStep = TotalSourceLines / 5
    FOR i = 1 TO TotalSourceLines
        SOURCEFILE = SOURCEFILE + SOURCECODE(i)
        k$ = INKEY$
        IF k$ = CHR$(27) THEN GOTO ProcessingCanceled
        IF i > UpdateStep THEN
            UpdateStep = UpdateStep + (TotalSourceLines / 5)
            VerboseProgress = i
            StatusMessage = TRIM$(STR$(i)) + "/" + TRIM$(STR$(TotalSourceLines))
            GOSUB AddVerboseOutputLine
        END IF
    NEXT i
    CHECKSUM = ADLER32(SOURCEFILE, -1)

    MainModule = -1
    FirstExecutableLine = -1
    MainModuleEND = 0
    CurrentSubFunc$ = ""
    TotalOutputLines = 0
    ProcessLine = 0
    ThisLineHasBPControl = 0
    UpdateStep = TotalSourceLines / 10
    'Look for variables inside the main module and store information in VARIABLES()
    'and LOCALVARIABLES. If SYSTEM is found, inject cleanup procedures (also when main module ends):
    TOTALVARIABLES = 0
    ProcessStepDescription = "Parsing .BAS and injecting breakpoint control code..."
    MULTILINE_DIM = 0
    MULTILINE = 0
    DO
        k$ = INKEY$
        IF k$ = CHR$(27) THEN
            ProcessingCanceled:
            Message$ = ""
            Message$ = Message$ + "Processing canceled."
            PCOPY 0, 1
            MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MKI$(OK_ONLY), 1, 0)
            EXIT SUB
        END IF
        IF LEN(caseBkpNextVar$) = 0 THEN 'Read next line unless we're in the middle of processing a line
            ProcessLine = ProcessLine + 1
            IF ProcessLine > TotalSourceLines THEN EXIT DO

            IF ProcessLine > UpdateStep THEN
                UpdateStep = UpdateStep + (TotalSourceLines / 10)
                VerboseProgress = ProcessLine
                StatusMessage = TRIM$(STR$(ProcessLine)) + "/" + TRIM$(STR$(TotalSourceLines))
                GOSUB AddVerboseOutputLine
            END IF
            bkpSourceLine$ = SOURCECODE(ProcessLine) 'Read the next source line
            caseBkpSourceLine = TRIM$(STRIPCOMMENTS(bkpSourceLine$)) 'Generate a version without comments or extra spaces
            SourceLine = UCASE$(caseBkpSourceLine) 'Generate an all upper case version

            'Detect '$DYNAMIC OR REM $DYNAMIC and activate SKIPARRAYS (only static arrays are watchable)
            temp.Sourceline$ = UCASE$(TRIM$(bkpSourceLine$))
            IF LEFT$(temp.Sourceline$, 1) = "'" THEN
                temp.Sourceline$ = TRIM$(RIGHT$(temp.Sourceline$, LEN(temp.Sourceline$) - 1))
                IF LEFT$(temp.Sourceline$, 8) = "$DYNAMIC" THEN
                    SKIPARRAYS = -1
                END IF
            ELSEIF LEFT$(temp.Sourceline$, 4) = "REM " THEN
                temp.Sourceline$ = TRIM$(RIGHT$(temp.Sourceline$, LEN(temp.Sourceline$) - 4))
                IF LEFT$(temp.Sourceline$, 8) = "$DYNAMIC" THEN
                    SKIPARRAYS = -1
                END IF
            END IF

            'BREAKPOINTS: Handle exceptions - cases in which a call to vwatch64_CHECKBREAKPOINT is
            'not necessary (comments, CONST, etc...) or not allowed (CASE, TYPE, etc...):
            e1$ = GETELEMENT$(SourceLine, 1)
            e2$ = GETELEMENT$(SourceLine, 2)
            IF DefiningType = 0 AND InBetweenSubs = 0 AND DeclaringLibrary = 0 THEN
                IF LEN(SourceLine) = 0 THEN
                ELSEIF LEFT$(SourceLine, 1) = "$" THEN
                ELSEIF e1$ = "DIM" THEN
                ELSEIF e1$ = "COMMON" THEN
                ELSEIF e1$ = "SUB" THEN
                ELSEIF e1$ = "DATA" THEN
                ELSEIF e1$ = "CASE" THEN
                ELSEIF e1$ = "TYPE" THEN
                ELSEIF e1$ = "REDIM" THEN
                ELSEIF e1$ = "CONST" THEN
                ELSEIF e1$ = "STATIC" THEN
                ELSEIF e1$ = "DEFINT" THEN
                ELSEIF e1$ = "DEFLNG" THEN
                ELSEIF e1$ = "DEFSTR" THEN
                ELSEIF e1$ = "DEFSNG" THEN
                ELSEIF e1$ = "DEFDBL" THEN
                ELSEIF e1$ = "DECLARE" THEN
                ELSEIF e1$ = "_DEFINE" THEN
                ELSEIF e1$ = "FUNCTION" THEN
                ELSEIF e1$ = "END" AND (e2$ = "DECLARE") THEN
                ELSEIF e1$ = "END" AND (e2$ = "SELECT") THEN
                ELSEIF e1$ = "OPTION" AND (e2$ = "_EXPLICIT") THEN
                ELSE
                    IF e1$ = "END" AND (e2$ = "FUNCTION" OR e2$ = "SUB") THEN
                        SkipStatement$ = "vWATCH64_DUMMY%% = 0"
                    ELSE
                        SkipStatement$ = "GOTO vwatch64_SKIP_" + LTRIM$(STR$(ProcessLine))
                    END IF
                    IF PrecompilerBlock = 0 AND CheckingOff = 0 AND MULTILINE = 0 THEN
                        IF FirstExecutableLine THEN FirstExecutableLine = 0
                        GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine)) + ":::: " + IIFSTR$(MainModule = 0, "GOSUB vwatch64_VARIABLEWATCH: ", "") + "vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(" + TRIM$(STR$(ProcessLine)) + "): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN " + SkipStatement$ + " ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine))
                        IF SkipStatement$ = "vWATCH64_DUMMY%% = 0" THEN ThisLineHasBPControl = 0 ELSE ThisLineHasBPControl = ProcessLine
                        GOSUB AddNextLineData
                    ELSE
                        IF FirstExecutableLine THEN
                            'Even if all conditions above indicate we shouldn't inject breakpoint
                            'code in this line, we'll inject it anyway if it's the first executable
                            'line we found in the source code, so that we can start paused.
                            FirstExecutableLine = 0
                            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine)) + ":::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(" + TRIM$(STR$(ProcessLine)) + "): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE"
                            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ":::: IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine))
                        END IF
                    END IF
                END IF
            END IF
            IF RIGHT$(SourceLine, 1) = "_" THEN MULTILINE = -1 ELSE MULTILINE = 0
        ELSE
            NextVar$ = UCASE$(caseBkpNextVar$)
        END IF

        IF DefiningType THEN
            'A TYPE ... was found earlier, so until we find an END TYPE, we'll populate UDT():
            IF LEFT$(SourceLine, 8) = "END TYPE" THEN
                DefiningType = 0
            ELSE
                IF INSTR(SourceLine, " AS ") > 0 THEN
                    TotalUDTs = TotalUDTs + 1
                    REDIM _PRESERVE UDT(1 TO TotalUDTs) AS UDTTYPE
                    UDT(TotalUDTs).UDT = KeywordList(TotalKeywords)
                    UDT(TotalUDTs).ELEMENT = LEFT$(caseBkpSourceLine, INSTR(UCASE$(SourceLine), " AS "))
                    UDT(TotalUDTs).DATATYPE = RIGHT$(caseBkpSourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3)
                END IF
            END IF
        END IF

        IF MULTILINE_DIM THEN SourceLine = IIFSTR$(LocalVariable, "DIM ", "DIM SHARED ") + SourceLine: MULTILINE_DIM = 0

        'IF DIM or equivalent appear somewhere in the line, but not the beginning,
        'change SourceLine to begin at such statements, for easier parsing for variables:
        FindDIM = 0
        IF FIND_KEYWORD(SourceLine, "DIM", FindDIM) THEN
            IF FindDIM > 1 THEN SourceLine = MID$(SourceLine, FindDIM): caseBkpSourceLine = MID$(caseBkpSourceLine, FindDIM)
        ELSEIF FIND_KEYWORD(SourceLine, "COMMON", FindDIM) THEN
            IF FindDIM > 1 THEN SourceLine = MID$(SourceLine, FindDIM): caseBkpSourceLine = MID$(caseBkpSourceLine, FindDIM)
        ELSEIF FIND_KEYWORD(SourceLine, "STATIC", FindDIM) AND NOT MainModule THEN
            IF FindDIM > 1 THEN SourceLine = MID$(SourceLine, FindDIM): caseBkpSourceLine = MID$(caseBkpSourceLine, FindDIM)
        END IF

        IF LEFT$(SourceLine, 4) = "DIM " OR LEFT$(SourceLine, 7) = "COMMON " OR (LEFT$(SourceLine, 7) = "STATIC " AND NOT MainModule) THEN
            LocalVariable = 0
            IF GETELEMENT$(SourceLine, 2) <> "SHARED" THEN LocalVariable = -1

            IF LEN(caseBkpNextVar$) > 0 THEN
                NextVar$ = UCASE$(caseBkpNextVar$)
            ELSE
                caseBkpNextVar$ = GETNEXTVARIABLE$(caseBkpSourceLine, ProcessLine)
                NextVar$ = UCASE$(caseBkpNextVar$)
            END IF

            IF INSTR(NextVar$, " AS ") = 0 THEN
                'Attempt to infer DATA TYPE from suffixes:
                FoundType = SUFFIXLOOKUP$(NextVar$)
                DefaultTypeUsed = 0

                IF LEN(FoundType) = 0 THEN
                    FoundType = DEFAULTDATATYPE(ASC(NextVar$, 1)) 'Assume default data type
                    DefaultTypeUsed = -1
                END IF

                IsArray = 0
                IF INSTR(NextVar$, "(") THEN IsArray = -1: PARSEARRAY NextVar$, ValidArray%, LowerBoundary%, UpperBoundary%
                IF IsArray THEN
                    IF ValidArray% AND NOT SKIPARRAYS THEN
                        ValidArray% = 0
                        FOR i = LowerBoundary% TO UpperBoundary%
                            TOTALVARIABLES = TOTALVARIABLES + 1
                            REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                            VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(caseBkpNextVar$, "(")) + TRIM$(STR$(i)) + ")"
                            IF MainModule THEN
                                VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                            ELSE
                                VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                            END IF
                            VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                        NEXT i
                    END IF
                ELSE
                    TOTALVARIABLES = TOTALVARIABLES + 1
                    REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                    VARIABLES(TOTALVARIABLES).NAME = caseBkpNextVar$
                    IF MainModule THEN
                        VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                    ELSE
                        VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                    END IF
                    VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                END IF

                IF LocalVariable AND MainModule THEN
                    TotalLocalVariables = TotalLocalVariables + 1
                    REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                    LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                    LOCALVARIABLES(TotalLocalVariables).DATATYPE = IIFSTR$(DefaultTypeUsed, "", VARIABLES(TOTALVARIABLES).DATATYPE)
                END IF
            ELSE
                FoundType = RIGHT$(NextVar$, LEN(NextVar$) - INSTR(NextVar$, " AS ") - 3)
                IF INSTR(FoundType, "STRING") > 0 THEN FoundType = "STRING"

                IF CHECKLIST(FoundType, KeywordList(), INTERNALKEYWORDS) THEN
                    'Variable is defined as an internal DATA TYPE.
                    IsArray = 0
                    IF INSTR(NextVar$, "(") THEN IsArray = -1: PARSEARRAY NextVar$, ValidArray%, LowerBoundary%, UpperBoundary%
                    IF IsArray THEN
                        IF ValidArray% AND NOT SKIPARRAYS THEN
                            ValidArray% = 0
                            FOR i = LowerBoundary% TO UpperBoundary%
                                TOTALVARIABLES = TOTALVARIABLES + 1
                                REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                                VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(caseBkpNextVar$, "(")) + TRIM$(STR$(i)) + ")"
                                IF MainModule THEN
                                    VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                                ELSE
                                    VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                                END IF
                                VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                            NEXT i
                        END IF
                    ELSE
                        TOTALVARIABLES = TOTALVARIABLES + 1
                        REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                        VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1)
                        IF MainModule THEN
                            VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                        ELSE
                            VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                        END IF
                        VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                    END IF

                    IF LocalVariable AND MainModule THEN
                        TotalLocalVariables = TotalLocalVariables + 1
                        REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                        LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                        LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                    END IF
                ELSE
                    'Variable is defined as a user defined type.
                    IsArray = 0
                    IF INSTR(NextVar$, "(") THEN IsArray = -1: PARSEARRAY NextVar$, ValidArray%, LowerBoundary%, UpperBoundary%
                    IF IsArray THEN
                        IF ValidArray% AND NOT SKIPARRAYS THEN
                            ValidArray% = 0
                            FOR ItemsinArray = LowerBoundary% TO UpperBoundary%
                                FOR i = 1 TO TotalUDTs
                                    'Expand variables defined as UDTs to Variable(?).Element format:
                                    IF UCASE$(TRIM$(UDT(i).UDT)) = FoundType THEN
                                        TOTALVARIABLES = TOTALVARIABLES + 1
                                        REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                                        VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, "(")) + TRIM$(STR$(ItemsinArray)) + ")." + TRIM$(UDT(i).ELEMENT)
                                        IF MainModule THEN
                                            VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                                        ELSE
                                            VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                                        END IF
                                        VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                        VARIABLES(TOTALVARIABLES).DATATYPE = TRIM$(UDT(i).DATATYPE)

                                        IF LocalVariable AND MainModule THEN
                                            TotalLocalVariables = TotalLocalVariables + 1
                                            REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                            LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                            LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                        END IF
                                    END IF
                                NEXT i
                            NEXT ItemsinArray
                        END IF
                    ELSE
                        'Expand variables defined as UDTs to Variable.Element format:
                        TotalExpandedWithUDT = TotalExpandedWithUDT + 1
                        REDIM _PRESERVE ExpandedWithUDT(1 TO TotalExpandedWithUDT) AS VARIABLESTYPE
                        ExpandedWithUDT(TotalExpandedWithUDT).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1)
                        IF MainModule THEN
                            ExpandedWithUDT(TotalExpandedWithUDT).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                        ELSE
                            ExpandedWithUDT(TotalExpandedWithUDT).SCOPE = CurrentSubFunc$
                        END IF

                        FOR i = 1 TO TotalUDTs
                            IF UCASE$(TRIM$(UDT(i).UDT)) = FoundType THEN
                                TOTALVARIABLES = TOTALVARIABLES + 1
                                REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                                VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1) + "." + TRIM$(UDT(i).ELEMENT)
                                IF MainModule THEN
                                    VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "MAIN MODULE", "SHARED")
                                ELSE
                                    VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                                END IF
                                VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                VARIABLES(TOTALVARIABLES).DATATYPE = TRIM$(UDT(i).DATATYPE)

                                IF LocalVariable AND MainModule THEN
                                    TotalLocalVariables = TotalLocalVariables + 1
                                    REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                    LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                    LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                END IF
                            END IF
                        NEXT i
                    END IF
                END IF
            END IF
            caseBkpNextVar$ = GETNEXTVARIABLE$(caseBkpSourceLine, ProcessLine)
            IF LEN(caseBkpNextVar$) = 0 THEN
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
                IF RIGHT$(SourceLine, 1) = "_" THEN MULTILINE_DIM = -1
            END IF
        ELSEIF LEFT$(SourceLine, 8) = "DECLARE " THEN
            IF INSTR(SourceLine, " LIBRARY") THEN
                DeclaringLibrary = -1
                FoundQuote = INSTR(SourceLine, CHR$(34))
                FoundClosingQuote = INSTR(FoundQuote + 1, SourceLine, CHR$(34))
                IF FoundQuote THEN
                    LibName$ = UCASE$(MID$(SourceLine, FoundQuote + 1, FoundClosingQuote - FoundQuote - 1))
                ELSE
                    LibName$ = ""
                END IF
            END IF
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 11) = "END DECLARE" THEN
            DeclaringLibrary = 0
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 16) = "OPTION _EXPLICIT" THEN
            SET_OPTION_EXPLICIT = -1
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "'OPTION _EXPLICIT"
        ELSEIF LEFT$(SourceLine, 13) = "OPTION BASE 1" THEN
            SET_OPTIONBASE = 1
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFINT " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "INTEGER"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFLNG " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "LONG"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSTR " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "STRING"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSNG " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "SINGLE"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFDBL " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "DOUBLE"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 8) = "_DEFINE " THEN
            IF INSTR(SourceLine, " AS ") > 0 THEN
                SET_DEF MID$(SourceLine, 9, INSTR(SourceLine, " AS ") - 9), RIGHT$(SourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3)
            END IF
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 5) = "TYPE " THEN
            'User defined types will be added to the DATA TYPE keyword list:
            ThisKeyword = RIGHT$(caseBkpSourceLine, LEN(SourceLine) - 5)
            GOSUB AddThisKeyword
            DefiningType = -1
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 4) = "$IF " THEN
            'No vWATCH64 labels will be placed/accessible inside precompiler blocks
            'otherwise vWATCH64 would have to evaluate them and that's QB64's job.
            PrecompilerBlock = -1
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "$END IF" THEN
            PrecompilerBlock = 0
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 13) = "$CHECKING:OFF" OR UCASE$(LEFT$(TRIM$(bkpSourceLine$), 13)) = "'VWATCH64:OFF" THEN
            'vWATCH64 won't mess with lines of code between $CHECKING:OFF
            'and $CHECKING:ON, considering that such lines are technically
            'error free, by the programmer's own judgement to include such
            'metacommands. Custom metacommand 'VWATCH64:OFF will also make
            'lines be ignored until a 'VWATCH64:ON is found.
            CheckingOff = -1
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 12) = "$CHECKING:ON" OR UCASE$(LEFT$(TRIM$(bkpSourceLine$), 12)) = "'VWATCH64:ON" THEN
            CheckingOff = 0
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 4) = "SUB " THEN
            InBetweenSubs = 0
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    GOSUB AddEndOfMainModuleCode
                    GOSUB AddGotoNextLineCode
                    MainModuleEND = TotalOutputLines
                END IF
                TotalNextLineData = 0
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1"
                IF INSTR(SourceLine, "(") THEN
                    CurrentSubFunc$ = TRIM$("SUB " + MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5))
                    Dummy$ = GETNEXTVARIABLE$("", -2) 'Reset STATIC variables in function GETNEXTVARIABLE$
                    GOSUB AddSFParametersAsVariables
                    StatusMessage = "Found: SUB " + MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5)
                    GOSUB AddVerboseOutputLine
                ELSE
                    CurrentSubFunc$ = caseBkpSourceLine
                    StatusMessage = "Found: " + caseBkpSourceLine
                    GOSUB AddVerboseOutputLine
                END IF
                TotalSubFunc = TotalSubFunc + 1
                REDIM _PRESERVE SUBFUNC(1 TO TotalSubFunc) AS SUBFUNC_TYPE
                SUBFUNC(TotalSubFunc).NAME = CurrentSubFunc$
                SUBFUNC(TotalSubFunc).LINE = ProcessLine
            ELSE
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
            END IF
        ELSEIF LEFT$(SourceLine, 9) = "FUNCTION " THEN
            InBetweenSubs = 0
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    GOSUB AddEndOfMainModuleCode
                    GOSUB AddGotoNextLineCode
                    MainModuleEND = TotalOutputLines
                END IF
                TotalNextLineData = 0
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1"
                IF INSTR(SourceLine, "(") THEN
                    CurrentSubFunc$ = TRIM$("FUNCTION " + MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10))
                    Dummy$ = GETNEXTVARIABLE$("", -2) 'Reset STATIC variables in function GETNEXTVARIABLE$
                    GOSUB AddSFParametersAsVariables
                    StatusMessage = "Found: FUNCTION " + MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10)
                    GOSUB AddVerboseOutputLine
                ELSE
                    CurrentSubFunc$ = caseBkpSourceLine
                    StatusMessage = "Found: FUNCTION " + caseBkpSourceLine
                    GOSUB AddVerboseOutputLine
                END IF
                TotalSubFunc = TotalSubFunc + 1
                REDIM _PRESERVE SUBFUNC(1 TO TotalSubFunc) AS SUBFUNC_TYPE
                SUBFUNC(TotalSubFunc).NAME = CurrentSubFunc$
                SUBFUNC(TotalSubFunc).LINE = ProcessLine
            ELSE
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
            END IF
        ELSEIF LEFT$(SourceLine, 7) = "END SUB" OR LEFT$(SourceLine, 12) = "END FUNCTION" THEN
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1"
            IF INSTR(SourceLine, "END SUB") > 0 THEN
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "EXIT SUB"
            ELSEIF INSTR(SourceLine, "END FUNCTION") > 0 THEN
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "EXIT FUNCTION"
            END IF
            GOSUB AddGotoNextLineCode
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
            SUBFUNC(TotalSubFunc).ENDING = TotalOutputLines
            InBetweenSubs = -1
            CurrentSubFunc$ = ""
        ELSEIF SourceLine = "SYSTEM" OR SourceLine = "END" THEN
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "ON ERROR GOTO vwatch64_FILEERROR"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "IF vwatch64_HEADER.CONNECTED THEN"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    vwatch64_HEADER.CONNECTED = 0"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "END IF"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "CLOSE #vwatch64_CLIENTFILE"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "KILL " + Q$ + _CWD$ + PATHSEP$ + "vwatch64.dat" + Q$
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "ON ERROR GOTO 0"
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        ELSEIF SourceLine = "STOP" THEN
            'STOP will be handled by vWATCH64 and not the QB64 compiler, so that it can be
            'converted into an automatic breakpoint, instead of quitting the program.
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vWATCH64_DUMMY%% = 0 'STOP"
        ELSEIF SourceLine = "$CONSOLE:ONLY" THEN
            'CONSOLE:ONLY causes issues with vWATCH64; For compatibility purposes,
            'it'll be turned into $CONSOLE
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "$CONSOLE"
        ELSEIF FIND_KEYWORD(bkpSourceLine$, "EXIT SUB", Found_ES_At) OR FIND_KEYWORD(bkpSourceLine$, "EXIT FUNCTION", Found_EF_At) THEN
            GOSUB AddOutputLine
            IF Found_ES_At THEN
                OutputLines(TotalOutputLines) = LEFT$(bkpSourceLine$, Found_ES_At - 1) + "vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1: " + MID$(bkpSourceLine$, Found_ES_At)
            ELSEIF Found_EF_At THEN
                OutputLines(TotalOutputLines) = LEFT$(bkpSourceLine$, Found_EF_At - 1) + "vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1: " + MID$(bkpSourceLine$, Found_EF_At)
            END IF
        ELSE
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
        END IF
        IF ThisLineHasBPControl > 0 AND MULTILINE = 0 AND InBetweenSubs = 0 THEN
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_SKIP_" + LTRIM$(STR$(ThisLineHasBPControl)) + ":::: "
            ThisLineHasBPControl = 0
        END IF
    LOOP

    'After all source was processed, we'll parse it once again looking for
    'temporary variables - those not initialized/defined with DIM/STATIC.
    ProcessStepDescription = "Parsing source for non-initialized variables..."

    CurrSF = 0
    ProcessLine = 0
    SET_DEF "A-Z", "SINGLE"
    UpdateStep = TotalSourceLines / 10
    DO
        SEP$ = "<> "
        ProcessLine = ProcessLine + 1
        IF ProcessLine > TotalSourceLines THEN EXIT DO
        k$ = INKEY$: IF k$ = CHR$(27) THEN GOTO ProcessingCanceled
        IF ProcessLine > UpdateStep THEN
            UpdateStep = UpdateStep + (TotalSourceLines / 10)
            VerboseProgress = ProcessLine
            StatusMessage = TRIM$(STR$(ProcessLine)) + "/" + TRIM$(STR$(TotalSourceLines))
            GOSUB AddVerboseOutputLine
        END IF

        IF CurrSF < TotalSubFunc THEN
            IF ProcessLine >= SUBFUNC(CurrSF + 1).LINE THEN CurrSF = CurrSF + 1
        END IF

        IF CurrSF = 0 THEN MainModule = -1 ELSE MainModule = 0

        SourceLine = TRIM$(STRIPCOMMENTS(SOURCECODE(ProcessLine))) 'Read the next source line
        uSourceLine$ = UCASE$(SourceLine)

        IF LEFT$(uSourceLine$, 7) = "DEFINT " THEN
            SET_DEF RIGHT$(uSourceLine$, LEN(uSourceLine$) - 7), "INTEGER"
        ELSEIF LEFT$(uSourceLine$, 7) = "DEFLNG " THEN
            SET_DEF RIGHT$(uSourceLine$, LEN(uSourceLine$) - 7), "LONG"
        ELSEIF LEFT$(uSourceLine$, 7) = "DEFSTR " THEN
            SET_DEF RIGHT$(uSourceLine$, LEN(uSourceLine$) - 7), "STRING"
        ELSEIF LEFT$(uSourceLine$, 7) = "DEFSNG " THEN
            SET_DEF RIGHT$(uSourceLine$, LEN(uSourceLine$) - 7), "SINGLE"
        ELSEIF LEFT$(uSourceLine$, 7) = "DEFDBL " THEN
            SET_DEF RIGHT$(uSourceLine$, LEN(uSourceLine$) - 7), "DOUBLE"
        ELSEIF LEFT$(uSourceLine$, 8) = "_DEFINE " THEN
            IF INSTR(uSourceLine$, " AS ") > 0 THEN
                SET_DEF MID$(uSourceLine$, 9, INSTR(uSourceLine$, " AS ") - 9), RIGHT$(uSourceLine$, LEN(uSourceLine$) - INSTR(uSourceLine$, " AS ") - 3)
            END IF
        END IF

        StartPos = 0
        ProcessingLineInput = 0
        DO
            StartPos = FIND_SYMBOL(StartPos + 1, SourceLine, "=")
            Start = StartPos
            IF Start > 0 THEN
                Start = Start - 1
                IF Start > 1 THEN
                    DO UNTIL INSTR(SEP$, MID$(SourceLine, Start, 1)) = 0
                        'Treat absurd cases of bad formatting like "x        =1"
                        Start = Start - 1
                    LOOP
                END IF
                'Read backwards from here until we find the beginning of the var name:
                SEP$ = " :"
                FOR i = Start TO 1 STEP -1
                    IF INSTR(SEP$, MID$(SourceLine, i, 1)) > 0 THEN Found = i: EXIT FOR
                NEXT i
                IF i = 0 THEN Found = 1 'No separator was found, but we reached the beginning of the line

                caseBkpNextVar$ = TRIM$(MID$(SourceLine, Found, Start - Found + 1))
                AnalyzeThisVar:
                NextVar$ = UCASE$(caseBkpNextVar$)

                'No _MEMNEW variables allowed:
                VarAssignment$ = ""
                BracketFound = INSTR(StartPos, SourceLine, "(")
                IF BracketFound THEN
                    VarAssignment$ = UCASE$(TRIM$(MID$(SourceLine, StartPos + 1, BracketFound - StartPos - 1)))
                END IF
                IF VarAssignment$ = "_MEMNEW" THEN GOTO NoValidVarFound

                Start = Found

                'No arrays:
                IF INSTR(NextVar$, "(") > 0 OR INSTR(NextVar$, ")") > 0 THEN GOTO NoValidVarFound

                'Variable names must start with A-Z, a-z
                IF ASC(NextVar$, 1) < 65 OR ASC(NextVar$, 1) > 90 THEN GOTO NoValidVarFound

                'NextVar$ can't be an internal keyword
                IF IS_KEYWORD(NextVar$) THEN GOTO NoValidVarFound

                'NextVar$ can't be a FUNCTION name:
                FOR i = 1 TO UBOUND(SUBFUNC)
                    IF GETELEMENT$(SUBFUNC(i).NAME, 1) = "FUNCTION" THEN
                        IF REMOVESIGIL$(NextVar$) = REMOVESIGIL$(UCASE$(GETELEMENT$(SUBFUNC(i).NAME, 2))) THEN GOTO NoValidVarFound
                    END IF
                NEXT

                'Check for var AS UDT being used as var = var2 (copying values)
                FOR i = 1 TO UBOUND(ExpandedWithUDT)
                    IF NextVar$ = UCASE$(RTRIM$(ExpandedWithUDT(i).NAME)) THEN
                        IF RTRIM$(ExpandedWithUDT(i).SCOPE) = "MAIN MODULE" AND MainModule OR _
                           RTRIM$(ExpandedWithUDT(i).SCOPE) = "SHARED" AND NOT MainModule THEN
                            GOTO NoValidVarFound
                        ELSEIF RTRIM$(ExpandedWithUDT(i).SCOPE) = TRIM$(SUBFUNC(CurrSF).NAME) THEN
                            GOTO NoValidVarFound
                        END IF
                    END IF
                NEXT

                'Check if this is actually a CONST:
                'CONST TRUE = -1: CONST FALSE = NOT TRUE
                FoundCONST = FIND_KEYWORD(SourceLine, "CONST", FoundCONSTAt)
                IF FoundCONST AND (FoundCONSTAt < Start) THEN
                    'It's a const.
                    GOTO NoValidVarFound
                END IF

                AllCriteriaMet:
                GOSUB AddThisTempVar
                IF ProcessingLineInput THEN RETURN
            ELSE
                'Look for keywords that create variables, without an assignment (=)
                bkpSourceLine$ = SourceLine
                SpecialKeyword = 0
                DO
                    SpecialKeyword = SpecialKeyword + 1
                    SELECT CASE SpecialKeyword
                        CASE 1: ThisKeyword$ = "INPUT"
                        CASE 2: ThisKeyword$ = "READ"
                        CASE 3: ThisKeyword$ = "GET"
                        CASE 4: ThisKeyword$ = "FIELD"
                        CASE ELSE: EXIT DO
                    END SELECT

                    SourceLine = bkpSourceLine$

                    StartSpecialParsing:
                    FoundSpecialKeyword = FIND_KEYWORD(SourceLine, ThisKeyword$, SpecialKeywordFoundAt)
                    IF FoundSpecialKeyword THEN
                        IF SpecialKeywordFoundAt = 0 THEN SpecialKeywordFoundAt = 1
                        SourceLine = MID$(SourceLine, SpecialKeywordFoundAt + 5)
                        ElementCount = 0
                        ProcessingLineInput = -1
                        DO
                            DO
                                ElementCount = ElementCount + 1
                                ThisElement$ = GETELEMENT$(SourceLine, ElementCount)
                                ThisElementPosition = GETELEMENT_LASTPOSITION
                                NextElement$ = GETELEMENT(SourceLine, ElementCount + 1)
                                IF LEFT$(ThisElement$, 1) <> CHR$(34) THEN EXIT DO
                            LOOP
                            caseBkpNextVar$ = ThisElement$
                            IF LEN(caseBkpNextVar$) THEN
                                IF IS_KEYWORD(caseBkpNextVar$) THEN
                                    IF ThisKeyword$ = "FIELD" AND UCASE$(caseBkpNextVar$) = "AS" THEN
                                        'Ignore this specific case
                                    ELSE
                                        'Consider that the current statement is over, move on:
                                        SourceLine = MID$(SourceLine, INSTR(SourceLine, caseBkpNextVar$) + LEN(caseBkpNextVar$))
                                        GOTO StartSpecialParsing
                                    END IF
                                END IF
                                IF ThisKeyword$ = "FIELD" AND UCASE$(NextElement$) = "AS" THEN
                                    'Ignore this specific case
                                ELSE
                                    IF MID$(SourceLine, ThisElementPosition, 1) = "(" THEN
                                        caseBkpNextVar$ = caseBkpNextVar$ + "()"
                                    END IF
                                    GOSUB AnalyzeThisVar
                                END IF
                            ELSE
                                EXIT DO
                            END IF
                        LOOP
                    END IF
                LOOP
                ProcessingLineInput = 0
            END IF
            NoValidVarFound:
            IF ProcessingLineInput THEN RETURN
        LOOP UNTIL StartPos = 0
    LOOP

    IF TOTALVARIABLES > 0 THEN
        Message$ = "Total watchable variables found:" + STR$(TOTALVARIABLES)
        IF TOTALVARIABLES > 50 THEN
            Message$ = Message$ + CHR$(LF) + "(watching too many variables will considerably slow your program down)"
        END IF
        MessageSetup$ = MKI$(MB_CUSTOM) + "Add all" + STR$(TOTALVARIABLES) + " variables" + CHR$(LF) + "Select from the list" + CHR$(LF) + "Cancel"
        PCOPY 0, 1
        MESSAGEBOX_RESULT = MESSAGEBOX("Processing done", Message$, MessageSetup$, IIF(TOTALVARIABLES > 100, 2, 1), 0)
        IF MESSAGEBOX_RESULT = 3 OR MESSAGEBOX_RESULT = -1 THEN EXIT SUB
        COLOR , _RGB32(230, 230, 230)
        _AUTODISPLAY
        IF MESSAGEBOX_RESULT = 2 THEN
            bkpx% = POS(1): bkpy% = CSRLIN
            BackupScreen = _COPYIMAGE(0)
            INTERACTIVE_MODE AddedList$, TotalSelected
            CLS , _RGB32(230, 230, 230)
            _AUTODISPLAY
            _PUTIMAGE (0, 0), BackupScreen
            _FREEIMAGE BackupScreen
            LOCATE bkpy%, bkpx%
            IF AddedList$ = CHR$(3) THEN
                'Processing was canceled by user.
                EXIT SUB
            END IF
        ELSE
            AddedList$ = STRING$(TOTALVARIABLES, 1)
            TotalSelected = TOTALVARIABLES
        END IF
    END IF

    COLOR , _RGB32(230, 230, 230)
    _AUTODISPLAY

    IF MainModule THEN 'All lines have been parsed. This .BAS contains no SUBs/FUNCTIONs.
        MainModule = 0
        GOSUB AddEndOfMainModuleCode
        GOSUB AddGotoNextLineCode
        MainModuleEND = TotalOutputLines
    END IF
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ""

    OutputFile = FREEFILE
    OPEN NEWFILENAME$ FOR OUTPUT AS #OutputFile

    ProcessStepDescription = "Generating " + NOPATH$(NEWFILENAME$) + "..."
    VerboseMaxProgress = 100
    VerboseProgress = 0
    GOSUB AddVerboseOutputLine
    'Creates the output .vwatch.bas:
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "'vWATCH64 initialization code - version " + VERSION + ":"
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "DECLARE LIBRARY"
    PRINT #OutputFile, "    FUNCTION vwatch64_GETPID& ALIAS getpid ()"
    PRINT #OutputFile, "END DECLARE"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "DECLARE LIBRARY " + Q$ + "timers" + Q$
    PRINT #OutputFile, "    SUB VWATCH64_STOPTIMERS ALIAS stop_timers"
    PRINT #OutputFile, "    SUB VWATCH64_STARTTIMERS ALIAS start_timers"
    PRINT #OutputFile, "END DECLARE"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "CONST vwatch64_ID = " + Q$ + "vWATCH64" + Q$
    PRINT #OutputFile, "CONST vwatch64_VERSION = " + Q$ + RTRIM$(VERSION) + Q$
    PRINT #OutputFile, "CONST vwatch64_CHECKSUM = " + Q$ + CHECKSUM + Q$
    PRINT #OutputFile, "CONST vwatch64_FILENAME = " + Q$ + _CWD$ + PATHSEP$ + "vwatch64.dat" + Q$
    PRINT #OutputFile, ""
    PRINT #OutputFile, "'Breakpoint control:"
    PRINT #OutputFile, "CONST vwatch64_CONTINUE = 1"
    PRINT #OutputFile, "CONST vwatch64_NEXTSTEP = 2"
    PRINT #OutputFile, "CONST vwatch64_READY = 3"
    PRINT #OutputFile, "CONST vwatch64_SETVAR = 4"
    PRINT #OutputFile, "CONST vwatch64_SKIPSUB = 5"
    PRINT #OutputFile, "CONST vwatch64_SETNEXT = 7"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "TYPE vwatch64_HEADERTYPE"
    PRINT #OutputFile, "    CLIENT_ID AS STRING * 8"
    PRINT #OutputFile, "    VERSION AS STRING * 5"
    PRINT #OutputFile, "    CONNECTED AS _BYTE"
    PRINT #OutputFile, "    RESPONSE AS _BYTE"
    PRINT #OutputFile, "    PID AS LONG"
    PRINT #OutputFile, "END TYPE"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "TYPE vwatch64_CLIENTTYPE"
    PRINT #OutputFile, "    NAME AS STRING * 256"
    PRINT #OutputFile, "    CHECKSUM AS STRING * 8"
    PRINT #OutputFile, "    TOTALSOURCELINES AS LONG"
    PRINT #OutputFile, "    EXENAME AS STRING * 256"
    PRINT #OutputFile, "    LINENUMBER AS LONG"
    PRINT #OutputFile, "    TOTALVARIABLES AS LONG"
    PRINT #OutputFile, "    PID AS LONG"
    PRINT #OutputFile, "END TYPE"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "TYPE vwatch64_BREAKPOINTTYPE"
    PRINT #OutputFile, "    ACTION AS _BYTE"
    PRINT #OutputFile, "    LINENUMBER AS LONG"
    PRINT #OutputFile, "END TYPE"
    PRINT #OutputFile, ""
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, ""
        PRINT #OutputFile, "TYPE vwatch64_VARIABLESTYPE"
        PRINT #OutputFile, "    NAME AS STRING * 256"
        PRINT #OutputFile, "    SCOPE AS STRING * 50"
        PRINT #OutputFile, "    UDT AS STRING * 40"
        PRINT #OutputFile, "    DATATYPE AS STRING * 20"
        PRINT #OutputFile, "END TYPE"
        PRINT #OutputFile, ""
        PRINT #OutputFile, "TYPE vwatch64_VARIABLEVALUETYPE"
        PRINT #OutputFile, "    VALUE AS STRING * 256"
        PRINT #OutputFile, "END TYPE"
    END IF
    PRINT #OutputFile, ""
    PRINT #OutputFile, "DIM SHARED vwatch64_BREAKPOINT AS vwatch64_BREAKPOINTTYPE"
    PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINTCOMMAND AS vwatch64_BREAKPOINTTYPE"
    PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINTCOMMANDBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_BREAKPOINTBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_BREAKPOINTLISTBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_BREAKPOINTLIST AS STRING *" + STR$(TotalSourceLines)
    PRINT #OutputFile, "DIM SHARED vwatch64_CLIENT AS vwatch64_CLIENTTYPE"
    PRINT #OutputFile, "DIM SHARED vwatch64_CLIENTBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_CLIENTFILE AS INTEGER"
    PRINT #OutputFile, "DIM SHARED vwatch64_DATAINFOBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_DATABLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_EXCHANGEBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINTLISTBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINTEXPBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_HEADER AS vwatch64_HEADERTYPE"
    PRINT #OutputFile, "DIM SHARED vwatch64_HEADERBLOCK AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_USERQUIT AS _BYTE"
    PRINT #OutputFile, "DIM SHARED vwatch64_NEXTLINE AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_SUBLEVEL AS INTEGER"
    PRINT #OutputFile, "DIM SHARED vwatch64_TARGETVARINDEX AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_TIMER AS INTEGER"
    PRINT #OutputFile, "DIM SHARED vwatch64_EXCHANGEDATASIZE$4"
    PRINT #OutputFile, "DIM SHARED vwatch64_EXCHANGEDATA AS STRING"
    PRINT #OutputFile, "DIM SHARED vWATCH64_DUMMY%%"
    PRINT #OutputFile, ""
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, "DIM SHARED vwatch64_VARIABLES(1 TO " + LTRIM$(STR$(TotalSelected)) + ") AS vwatch64_VARIABLESTYPE"
        PRINT #OutputFile, "DIM SHARED vwatch64_VARIABLEDATA(1 TO " + LTRIM$(STR$(TotalSelected)) + ") AS vwatch64_VARIABLEVALUETYPE"
        PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINTLIST AS STRING *" + STR$(TotalSelected)
        PRINT #OutputFile, "DIM SHARED vwatch64_WATCHPOINT(1 TO " + LTRIM$(STR$(TotalSelected)) + ") AS vwatch64_VARIABLEVALUETYPE"
        tempindex = 0
        FOR i = 1 TO TOTALVARIABLES
            IF ASC(AddedList$, i) = 1 THEN
                tempindex = tempindex + 1
                PRINT #OutputFile, "vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").NAME = " + Q$ + TRIM$(VARIABLES(i).NAME) + Q$
                PRINT #OutputFile, "vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").SCOPE = " + Q$ + TRIM$(VARIABLES(i).SCOPE) + Q$
                PRINT #OutputFile, "vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").DATATYPE = " + Q$ + TRIM$(VARIABLES(i).DATATYPE) + Q$
            END IF
        NEXT i
        PRINT #OutputFile, ""
    END IF
    PRINT #OutputFile, "vwatch64_HEADERBLOCK = 1"
    PRINT #OutputFile, "vwatch64_CLIENTBLOCK = LEN(vwatch64_HEADER) + 1"
    PRINT #OutputFile, "vwatch64_BREAKPOINTBLOCK = vwatch64_CLIENTBLOCK + LEN(vwatch64_CLIENT) + 1"
    PRINT #OutputFile, "vwatch64_BREAKPOINTLISTBLOCK = vwatch64_BREAKPOINTBLOCK + LEN(vwatch64_BREAKPOINT) + 1"
    PRINT #OutputFile, "vwatch64_DATAINFOBLOCK = vwatch64_BREAKPOINTLISTBLOCK + LEN(vwatch64_BREAKPOINTLIST) + 1"
    PRINT #OutputFile, "vwatch64_DATABLOCK = vwatch64_DATAINFOBLOCK + LEN(vwatch64_VARIABLES()) + 1"
    PRINT #OutputFile, "vwatch64_WATCHPOINTLISTBLOCK = vwatch64_DATABLOCK + LEN(vwatch64_VARIABLEDATA()) + 1"
    PRINT #OutputFile, "vwatch64_WATCHPOINTEXPBLOCK = vwatch64_WATCHPOINTLISTBLOCK + LEN(vwatch64_WATCHPOINTLIST) + 1"
    PRINT #OutputFile, "vwatch64_WATCHPOINTCOMMANDBLOCK = vwatch64_WATCHPOINTEXPBLOCK + LEN(vwatch64_WATCHPOINT()) + 1"
    PRINT #OutputFile, "vwatch64_EXCHANGEBLOCK = vwatch64_WATCHPOINTCOMMANDBLOCK + LEN(vwatch64_WATCHPOINTCOMMAND) + 1"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "vwatch64_CONNECTTOHOST"
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, ""
        PRINT #OutputFile, "'Initialize the data export timer:"
        PRINT #OutputFile, "vwatch64_TIMER = _FREETIMER"
        PRINT #OutputFile, "ON TIMER(vwatch64_TIMER, .1) vwatch64_VARIABLEWATCH"
        PRINT #OutputFile, "TIMER(vwatch64_TIMER) ON"
    END IF
    PRINT #OutputFile, ""
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "'End of vWATCH64 initialization code."
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, ""

    VerboseProgress = 100
    GOSUB AddVerboseOutputLine

    ProcessStepDescription = "Dumping processed source code to disk..."
    StatusMessage = ""
    VerboseMaxProgress = TotalOutputLines
    UpdateStep = TotalOutputLines / 10
    'Dump the processed source into the output file.
    'Add end of SUB/FUNCTION variable watch.
    FOR i = 1 TO TotalOutputLines
        IF i = MainModuleEND THEN
            CurrentSubFunc$ = "MAIN MODULE"
            GOSUB AddSetVarCode
        END IF
        FOR j = 1 TO TotalSubFunc
            IF SUBFUNC(j).ENDING = i THEN
                CurrentSubFunc$ = TRIM$(SUBFUNC(j).NAME)
                GOSUB AddSFVariableWatchCode
            END IF
        NEXT j
        PRINT #OutputFile, OutputLines(i)
        IF i > UpdateStep THEN
            UpdateStep = UpdateStep + (TotalOutputLines / 10)
            VerboseProgress = i
            GOSUB AddVerboseOutputLine
        END IF
    NEXT i

    ProcessStepDescription = "Adding vWATCH64's custom procedures..."
    VerboseMaxProgress = 100
    VerboseProgress = 0
    GOSUB AddVerboseOutputLine

    'Add vWATCH64's procedures:
    PRINT #OutputFile, ""
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "'vWATCH64 procedures:"
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "SUB vwatch64_CONNECTTOHOST"
    RANDOMIZE TIMER
    PRINT #OutputFile, "    DIM k AS LONG"
    PRINT #OutputFile, "    DIM Message1$, Message2$, NoGo%"
    PRINT #OutputFile, "    DIM FileIsOpen%, FileExists%"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    vwatch64_CHECKFILE:"
    PRINT #OutputFile, "    IF _FILEEXISTS(vwatch64_FILENAME) = 0 THEN"
    PRINT #OutputFile, "        Message1$ = " + Q$ + "vWATCH64 doesn't seem to be running." + Q$
    PRINT #OutputFile, "        Message2$ = " + Q$ + "(Checking for 'vwatch64.dat'; ESC to cancel...)" + Q$
    PRINT #OutputFile, "        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "            _TITLE " + Q$ + "Connecting to vWATCH64..." + Q$
    PRINT #OutputFile, "            _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$"
    PRINT #OutputFile, "            _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$"
    PRINT #OutputFile, "        ELSE"
    PRINT #OutputFile, "            _CONSOLETITLE " + Q$ + "Connecting to vWATCH64..." + Q$
    PRINT #OutputFile, "            PRINT Message1$: PRINT Message1$"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        DO: _LIMIT 30"
    PRINT #OutputFile, "            k = _KEYHIT"
    PRINT #OutputFile, "            IF k = -27 THEN SYSTEM"
    PRINT #OutputFile, "            IF _FILEEXISTS(vwatch64_FILENAME) THEN _KEYCLEAR: EXIT DO"
    PRINT #OutputFile, "        LOOP"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    vwatch64_CLIENTFILE = " + LTRIM$(TRIM$(STR$(_CEIL(RND * 30000) + 100)))
    PRINT #OutputFile, "    OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Check if a connection is already active"
    PRINT #OutputFile, "    IF LOF(vwatch64_CLIENTFILE) > 0 THEN"
    PRINT #OutputFile, "        'Check if the file can be deleted; if so, vWATCH64 is not running."
    PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
    PRINT #OutputFile, "        NoGo% = 0"
    $IF WIN THEN
        PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "        KILL vwatch64_FILENAME"
        PRINT #OutputFile, "        ON ERROR GOTO 0"
        PRINT #OutputFile, "        NoGo% = _FILEEXISTS(vwatch64_FILENAME)"
    $ELSE
        PRINT #OutputFile, "        FileIsOpen% = _SHELLHIDE(" + Q$ + "lsof vwatch64.dat" + Q$ + ")"
        PRINT #OutputFile, "        FileExists% = _FILEEXISTS(vwatch64_FILENAME)"
        PRINT #OutputFile, "        IF FileIsOpen% = 127 THEN 'command LSOF not found."
        PRINT #OutputFile, "            FileIsOpen% = -1 'consider that vWATCH64 is running."
        PRINT #OutputFile, "        ELSEIF FileIsOpen% = 0 THEN 'file is in use."
        PRINT #OutputFile, "            FileIsOpen% = -1"
        PRINT #OutputFile, "        ELSEIF FileIsOpen% = 1 THEN 'file is not in use."
        PRINT #OutputFile, "            FileIsOpen% = 0 'consider that vWATCH64 is NOT running."
        PRINT #OutputFile, "        END IF"
        PRINT #OutputFile, "        IF FileExists% AND FileIsOpen% = 0 THEN"
        PRINT #OutputFile, "            ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "            KILL vwatch64_FILENAME"
        PRINT #OutputFile, "            ON ERROR GOTO 0"
        PRINT #OutputFile, "        ELSEIF FileExists% AND FileIsOpen% THEN"
        PRINT #OutputFile, "            NoGo% = -1"
        PRINT #OutputFile, "        END IF"
    $END IF
    PRINT #OutputFile, ""
    PRINT #OutputFile, "        IF NoGo% THEN"
    PRINT #OutputFile, "            CLS"
    PRINT #OutputFile, "            Message1$ = " + Q$ + "ERROR: vWATCH64 is already connected to another" + Q$
    PRINT #OutputFile, "            Message2$ = " + Q$ + "client/debuggee." + Q$
    PRINT #OutputFile, "            IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "                _TITLE " + Q$ + "FAILED!" + Q$
    PRINT #OutputFile, "                _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$"
    PRINT #OutputFile, "                _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$"
    PRINT #OutputFile, "            ELSE"
    PRINT #OutputFile, "                _CONSOLETITLE " + Q$ + "FAILED!" + Q$
    PRINT #OutputFile, "                PRINT Message1$: PRINT Message1$"
    PRINT #OutputFile, "            END IF"
    PRINT #OutputFile, "            END"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        GOTO vwatch64_CHECKFILE"
    PRINT #OutputFile, "    ELSEIF LOF(vwatch64_CLIENTFILE) = 0 THEN"
    PRINT #OutputFile, "        'Check if the file can be deleted; if so, vWATCH64 is not running."
    PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
    $IF WIN THEN
        PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "        KILL vwatch64_FILENAME"
        PRINT #OutputFile, "        ON ERROR GOTO 0"
        PRINT #OutputFile, "        IF _FILEEXISTS(vwatch64_FILENAME) = 0 THEN GOTO vwatch64_CHECKFILE"
    $ELSE
        PRINT #OutputFile, "        FileIsOpen% = _SHELLHIDE(" + Q$ + "lsof vwatch64.dat" + Q$ + ")"
        PRINT #OutputFile, "        IF FileIsOpen% = 127 THEN 'command LSOF not found."
        PRINT #OutputFile, "            FileIsOpen% = -1 'consider that vWATCH64 is running."
        PRINT #OutputFile, "        ELSEIF FileIsOpen% = 0 THEN 'file is in use."
        PRINT #OutputFile, "            FileIsOpen% = -1"
        PRINT #OutputFile, "        ELSEIF FileIsOpen% = 1 THEN 'file is not in use."
        PRINT #OutputFile, "            FileIsOpen% = 0 'consider that vWATCH64 is NOT running."
        PRINT #OutputFile, "        END IF"
        PRINT #OutputFile, "        IF FileIsOpen% = 0 THEN"
        PRINT #OutputFile, "            ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "            KILL vwatch64_FILENAME"
        PRINT #OutputFile, "            ON ERROR GOTO 0"
        PRINT #OutputFile, "            IF _FILEEXISTS(vwatch64_FILENAME) = 0 THEN GOTO vwatch64_CHECKFILE"
        PRINT #OutputFile, "        END IF"
    $END IF
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE"
    PRINT #OutputFile, "    vwatch64_CLIENT.NAME = " + Q$ + FILENAME$ + Q$
    PRINT #OutputFile, "    vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM"
    PRINT #OutputFile, "    vwatch64_CLIENT.TOTALSOURCELINES =" + STR$(TotalSourceLines)
    PRINT #OutputFile, "    vwatch64_CLIENT.TOTALVARIABLES =" + STR$(TotalSelected)
    PRINT #OutputFile, "    vwatch64_CLIENT.PID = vwatch64_GETPID&"
    PRINT #OutputFile, "    vwatch64_CLIENT.EXENAME = COMMAND$(0)"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Send this client's version and connection request"
    PRINT #OutputFile, "    vwatch64_HEADER.CLIENT_ID = vwatch64_ID"
    PRINT #OutputFile, "    vwatch64_HEADER.VERSION = vwatch64_VERSION"
    PRINT #OutputFile, "    vwatch64_HEADER.CONNECTED = -1"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_DATAINFOBLOCK, vwatch64_VARIABLES()"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Wait for authorization:"
    PRINT #OutputFile, "    CLS"
    PRINT #OutputFile, "    Message1$ = " + Q$ + "Waiting for authorization; ESC to cancel..." + Q$
    PRINT #OutputFile, "    IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "        _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$"
    PRINT #OutputFile, "    ELSE"
    PRINT #OutputFile, "        PRINT Message1$"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, "    DO: _LIMIT 30"
    PRINT #OutputFile, "        GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #OutputFile, "        k = _KEYHIT"
    PRINT #OutputFile, "        IF k = -27 THEN SYSTEM"
    PRINT #OutputFile, "     LOOP UNTIL vwatch64_HEADER.RESPONSE = -1 OR vwatch64_HEADER.CONNECTED = 0"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF vwatch64_HEADER.CONNECTED = 0 THEN"
    PRINT #OutputFile, "        SYSTEM"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    CLS"
    PRINT #OutputFile, "    IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "        _TITLE " + Q$ + "Untitled" + Q$
    PRINT #OutputFile, "    ELSE"
    PRINT #OutputFile, "        _CONSOLETITLE " + Q$ + "Untitled" + Q$
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT"
    PRINT #OutputFile, "END SUB"
    PRINT #OutputFile, ""
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, "SUB vwatch64_VARIABLEWATCH"
        LocalSharedAddedTotal = 0
        FOR i = 1 TO TotalLocalVariables
            SourceLine = "    SHARED "
            Found = FINDVARIABLES(1, TRIM$(LOCALVARIABLES(i).NAME), AddedList$)
            IF Found THEN
                IF LEN(TRIM$(VARIABLES(Found).UDT)) > 0 THEN
                    IF TotalUDTsAdded > 0 THEN
                        AlreadyAdded = 0
                        FOR L = 1 TO TotalUDTsAdded
                            IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                                IF TRIM$(UDT_ADDED(L).UDT) = TRIM$(VARIABLES(Found).UDT) AND LEFT$(VARIABLES(Found).NAME, INSTR(VARIABLES(Found).NAME, "(") - 1) = TRIM$(UDT_ADDED(L).NAME) THEN AlreadyAdded = -1
                            ELSE
                                IF TRIM$(UDT_ADDED(L).UDT) = TRIM$(VARIABLES(Found).UDT) AND TRIM$(VARIABLES(Found).NAME) = TRIM$(UDT_ADDED(L).NAME) THEN AlreadyAdded = -1
                            END IF
                        NEXT L
                        IF NOT AlreadyAdded THEN
                            'New local variable AS UDT found
                            TotalUDTsAdded = TotalUDTsAdded + 1
                            REDIM _PRESERVE UDT_ADDED(1 TO TotalUDTsAdded) AS VARIABLESTYPE
                            UDT_ADDED(TotalUDTsAdded).UDT = TRIM$(VARIABLES(Found).UDT)
                            IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                                SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, "(")) + ") AS " + TRIM$(VARIABLES(Found).UDT)
                                UDT_ADDED(TotalUDTsAdded).NAME = LEFT$(VARIABLES(Found).NAME, INSTR(VARIABLES(Found).NAME, "(") - 1)
                            ELSE
                                UDT_ADDED(TotalUDTsAdded).NAME = TRIM$(VARIABLES(Found).NAME)
                                SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, ".") - 1) + " AS " + TRIM$(VARIABLES(Found).UDT)
                            END IF
                            LocalSharedAddedTotal = LocalSharedAddedTotal + 1
                            REDIM _PRESERVE LOCALSHAREDADDED(1 TO LocalSharedAddedTotal) AS STRING
                            LOCALSHAREDADDED(LocalSharedAddedTotal) = SourceLine
                        END IF
                    ELSE
                        'New local variable AS UDT found
                        IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                            SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, "(")) + ") AS " + TRIM$(VARIABLES(Found).UDT)
                        ELSE
                            SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, ".") - 1) + " AS " + TRIM$(VARIABLES(Found).UDT)
                        END IF
                        TotalUDTsAdded = TotalUDTsAdded + 1
                        REDIM _PRESERVE UDT_ADDED(1 TO TotalUDTsAdded) AS VARIABLESTYPE
                        UDT_ADDED(TotalUDTsAdded).NAME = TRIM$(VARIABLES(Found).NAME)
                        UDT_ADDED(TotalUDTsAdded).UDT = TRIM$(VARIABLES(Found).UDT)
                        LocalSharedAddedTotal = LocalSharedAddedTotal + 1
                        REDIM _PRESERVE LOCALSHAREDADDED(1 TO LocalSharedAddedTotal) AS STRING
                        LOCALSHAREDADDED(LocalSharedAddedTotal) = SourceLine
                    END IF
                ELSE
                    IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                        IF LEN(SUFFIXLOOKUP$(TRIM$(LOCALVARIABLES(i).NAME))) > 0 OR TRIM$(LOCALVARIABLES(i).DATATYPE) = "" THEN
                            SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(TRIM$(LOCALVARIABLES(i).NAME), "(")) + ")"
                        ELSE
                            IF CHECKLIST(TRIM$(LOCALVARIABLES(i).DATATYPE), KeywordList(), INTERNALKEYWORDS) AND INSTR(SourceLine, " AS ") = 0 THEN
                                SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(TRIM$(LOCALVARIABLES(i).NAME), "(")) + ")" + " AS " + TRIM$(LOCALVARIABLES(i).DATATYPE)
                            END IF
                        END IF
                        LocalSharedAddedTotal = LocalSharedAddedTotal + 1
                        REDIM _PRESERVE LOCALSHAREDADDED(1 TO LocalSharedAddedTotal) AS STRING
                        LOCALSHAREDADDED(LocalSharedAddedTotal) = SourceLine
                    ELSE
                        IF LEN(SUFFIXLOOKUP$(TRIM$(LOCALVARIABLES(i).NAME))) > 0 OR TRIM$(LOCALVARIABLES(i).DATATYPE) = "" THEN
                            SourceLine = SourceLine + TRIM$(LOCALVARIABLES(i).NAME)
                        ELSE
                            IF CHECKLIST(TRIM$(LOCALVARIABLES(i).DATATYPE), KeywordList(), INTERNALKEYWORDS) AND INSTR(SourceLine, " AS ") = 0 THEN
                                SourceLine = SourceLine + TRIM$(LOCALVARIABLES(i).NAME) + " AS " + TRIM$(LOCALVARIABLES(i).DATATYPE)
                            END IF
                        END IF
                        LocalSharedAddedTotal = LocalSharedAddedTotal + 1
                        REDIM _PRESERVE LOCALSHAREDADDED(1 TO LocalSharedAddedTotal) AS STRING
                        LOCALSHAREDADDED(LocalSharedAddedTotal) = SourceLine
                    END IF
                END IF
            END IF
        NEXT i

        LocalSharedNotRepeated = 0
        REDIM LocalShared_NOREPETITION(1 TO LocalSharedAddedTotal) AS STRING
        FOR i = 1 TO LocalSharedAddedTotal
            Found = 0
            IF LocalSharedNotRepeated > 0 THEN
                FOR j = 1 TO LocalSharedNotRepeated
                    IF LOCALSHAREDADDED(i) = LocalShared_NOREPETITION(j) THEN Found = -1: EXIT FOR
                NEXT j
                IF NOT Found THEN
                    LocalSharedNotRepeated = LocalSharedNotRepeated + 1
                    LocalShared_NOREPETITION(LocalSharedNotRepeated) = LOCALSHAREDADDED(i)
                END IF
            ELSE
                LocalSharedNotRepeated = LocalSharedNotRepeated + 1
                LocalShared_NOREPETITION(LocalSharedNotRepeated) = LOCALSHAREDADDED(i)
            END IF
        NEXT i

        FOR i = 1 TO LocalSharedNotRepeated
            PRINT #OutputFile, LocalShared_NOREPETITION(i)
        NEXT i

        PRINT #OutputFile, ""
        PRINT #OutputFile, "    IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT SUB"
        PRINT #OutputFile, "    ON ERROR GOTO vwatch64_FILEERROR"
        FOR i = 1 TO TOTALVARIABLES
            IF ASC(AddedList$, i) = 1 AND (TRIM$(VARIABLES(i).SCOPE) = "MAIN MODULE" OR TRIM$(VARIABLES(i).SCOPE) = "SHARED") THEN
                IF INSTR(VARIABLES(i).DATATYPE, "STRING") THEN
                    SourceLine = "    vwatch64_VARIABLEDATA(" + LTRIM$(STR$(i)) + ").VALUE = " + TRIM$(VARIABLES(i).NAME)
                    PRINT #OutputFile, SourceLine
                ELSE
                    SourceLine = "    vwatch64_VARIABLEDATA(" + LTRIM$(STR$(i)) + ").VALUE = STR$(" + TRIM$(VARIABLES(i).NAME) + ")"
                    PRINT #OutputFile, SourceLine
                END IF
            END IF
        NEXT i
        PRINT #OutputFile, "    ON ERROR GOTO vwatch64_CLIENTFILEERROR"
        PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_DATABLOCK, vwatch64_VARIABLEDATA().VALUE"
        PRINT #OutputFile, "    ON ERROR GOTO 0"
        PRINT #OutputFile, "END SUB"
        PRINT #OutputFile, ""
    END IF
    PRINT #OutputFile, "FUNCTION vwatch64_CHECKBREAKPOINT&(LineNumber AS LONG)"
    PRINT #OutputFile, "    STATIC FirstRunDone AS _BYTE"
    PRINT #OutputFile, "    STATIC StepMode AS _BYTE"
    PRINT #OutputFile, "    STATIC StepAround AS _BYTE"
    PRINT #OutputFile, "    STATIC StartLevel AS INTEGER"
    PRINT #OutputFile, "    DIM k AS LONG"
    PRINT #OutputFile, "    DIM Message1$, Message2$"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF FirstRunDone = 0 THEN"
    PRINT #OutputFile, "        IF vwatch64_HEADER.CONNECTED = 0 THEN"
    PRINT #OutputFile, "            _DELAY .5"
    PRINT #OutputFile, "            IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "                _TITLE " + Q$ + "Untitled" + Q$
    PRINT #OutputFile, "            ELSE"
    PRINT #OutputFile, "                _CONSOLETITLE " + Q$ + "Untitled" + Q$
    PRINT #OutputFile, "            END IF"
    PRINT #OutputFile, "            FirstRunDone = -1"
    PRINT #OutputFile, "            EXIT FUNCTION"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "    ELSE"
    PRINT #OutputFile, "        IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT FUNCTION"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    vwatch64_CLIENT.LINENUMBER = LineNumber"
    PRINT #OutputFile, "    ON ERROR GOTO vwatch64_CLIENTFILEERROR"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Check if step mode was initiated by the host:"
    PRINT #OutputFile, "    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "    IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1"
    PRINT #OutputFile, "    IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    GOSUB vwatch64_PING"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Get the breakpoint list:"
    PRINT #OutputFile, "    vwatch64_BREAKPOINT.ACTION = vwatch64_READY"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTLISTBLOCK, vwatch64_BREAKPOINTLIST"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF StepAround = -1 AND vwatch64_SUBLEVEL > StartLevel AND (ASC(vwatch64_BREAKPOINTLIST, LineNumber) <> 1) THEN EXIT FUNCTION"
    PRINT #OutputFile, "    IF StepAround = -1 AND vwatch64_SUBLEVEL = StartLevel THEN StepAround = 0"
    PRINT #OutputFile, ""
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, "    vwatch64_VARIABLEWATCH"
        PRINT #OutputFile, "    IF vwatch64_CHECKWATCHPOINT = -1 THEN StepMode = -1"
    END IF
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'On the first time this procedure is called, execution is halted,"
    PRINT #OutputFile, "    'until the user presses F5 or F8 in vWATCH64"
    PRINT #OutputFile, "    IF FirstRunDone = 0 THEN"
    PRINT #OutputFile, "        Message1$ = " + Q$ + "Hit F8 to run line by line or switch to vWATCH64 and hit F5 to run;" + Q$
    PRINT #OutputFile, "        Message2$ = " + Q$ + "(ESC to quit)" + Q$
    PRINT #OutputFile, "        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "            _TITLE Message1$"
    PRINT #OutputFile, "            _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$"
    PRINT #OutputFile, "            _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$"
    PRINT #OutputFile, "        ELSE"
    PRINT #OutputFile, "            _CONSOLETITLE " + Q$ + "Switch to vWATCH64 and hit F5 to run or F8 to run line by line;" + Q$
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        VWATCH64_STOPTIMERS"
    PRINT #OutputFile, "        DO: _LIMIT 500"
    PRINT #OutputFile, "            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "            IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETNEXT THEN"
    PRINT #OutputFile, "                vwatch64_CHECKBREAKPOINT& = vwatch64_BREAKPOINT.LINENUMBER"
    PRINT #OutputFile, "                vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP"
    PRINT #OutputFile, "                vwatch64_BREAKPOINT.LINENUMBER = 0"
    PRINT #OutputFile, "                PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "                IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "                    _TITLE " + Q$ + "Untitled" + Q$ + ": CLS"
    PRINT #OutputFile, "                ELSE"
    PRINT #OutputFile, "                    _CONSOLETITLE " + Q$ + "Untitled" + Q$ + ": CLS"
    PRINT #OutputFile, "                END IF"
    PRINT #OutputFile, "                FirstRunDone = -1"
    PRINT #OutputFile, "                ON ERROR GOTO 0"
    PRINT #OutputFile, "                EXIT FUNCTION"
    PRINT #OutputFile, "            END IF"
    PRINT #OutputFile, "            k = _KEYHIT"
    PRINT #OutputFile, "            IF k = 16896 THEN vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP 'F8"
    PRINT #OutputFile, "            IF k = -27 THEN 'ESC"
    PRINT #OutputFile, "                CLOSE #vwatch64_CLIENTFILE"
    PRINT #OutputFile, "                SYSTEM"
    PRINT #OutputFile, "            END IF"
    PRINT #OutputFile, "            _KEYCLEAR"
    PRINT #OutputFile, "            GOSUB vwatch64_PING"
    PRINT #OutputFile, "        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP OR vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR OR vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1: StepAround = 0"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1: StepMode = -1"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR THEN"
    PRINT #OutputFile, "            vwatch64_CHECKBREAKPOINT& = -1"
    PRINT #OutputFile, "            StepMode = -1"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN"
    PRINT #OutputFile, "            _TITLE " + Q$ + "Untitled" + Q$ + ": CLS"
    PRINT #OutputFile, "        ELSE"
    PRINT #OutputFile, "            _CONSOLETITLE " + Q$ + "Untitled" + Q$ + ": CLS"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        FirstRunDone = -1"
    PRINT #OutputFile, "        ON ERROR GOTO 0"
    PRINT #OutputFile, "        VWATCH64_STARTTIMERS"
    PRINT #OutputFile, "        EXIT FUNCTION"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF (ASC(vwatch64_BREAKPOINTLIST, LineNumber) = 2) THEN"
    PRINT #OutputFile, "            vwatch64_CHECKBREAKPOINT& = -2"
    PRINT #OutputFile, "            EXIT FUNCTION"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF (ASC(vwatch64_BREAKPOINTLIST, LineNumber) = 1) OR (StepMode = -1) THEN"
    PRINT #OutputFile, "        VWATCH64_STOPTIMERS"
    PRINT #OutputFile, "        StepMode = -1"
    PRINT #OutputFile, "        DO: _LIMIT 500"
    PRINT #OutputFile, "            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "            IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETNEXT THEN"
    PRINT #OutputFile, "                vwatch64_CHECKBREAKPOINT& = vwatch64_BREAKPOINT.LINENUMBER"
    PRINT #OutputFile, "                vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP"
    PRINT #OutputFile, "                vwatch64_BREAKPOINT.LINENUMBER = 0"
    PRINT #OutputFile, "                StepMode = -1"
    PRINT #OutputFile, "                PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "                ON ERROR GOTO 0"
    PRINT #OutputFile, "                EXIT FUNCTION"
    PRINT #OutputFile, "            END IF"
    PRINT #OutputFile, "            k = _KEYHIT"
    PRINT #OutputFile, "            IF k = 16896 THEN vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP 'F8"
    PRINT #OutputFile, "            _KEYCLEAR"
    PRINT #OutputFile, "            GOSUB vwatch64_PING"
    PRINT #OutputFile, "        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP OR vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR OR vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE THEN StepMode = 0: StepAround = 0"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepAround = 0: StepMode = -1"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1: StepMode = -1"
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR THEN"
    PRINT #OutputFile, "            vwatch64_CHECKBREAKPOINT& = -1"
    PRINT #OutputFile, "            StepMode = -1"
    PRINT #OutputFile, "        END IF"
    PRINT #OutputFile, "        VWATCH64_STARTTIMERS"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    ON ERROR GOTO 0"
    PRINT #OutputFile, "    EXIT FUNCTION"
    PRINT #OutputFile, "    vwatch64_PING:"
    PRINT #OutputFile, "    'Check if connection is still alive on host's end"
    PRINT #OutputFile, "    GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #OutputFile, "    IF vwatch64_HEADER.CONNECTED = 0 THEN"
    PRINT #OutputFile, "        CLOSE vwatch64_CLIENTFILE"
    PRINT #OutputFile, "        IF FirstRunDone = 0 THEN FirstRunDone = -1: CLS: _TITLE " + Q$ + "Untitled" + Q$
    PRINT #OutputFile, "        VWATCH64_STARTTIMERS"
    PRINT #OutputFile, "        EXIT FUNCTION"
    PRINT #OutputFile, "    END IF"
    PRINT #OutputFile, "    RETURN"
    PRINT #OutputFile, "END SUB"
    PRINT #OutputFile, ""
    PRINT #OutputFile, ""
    IF TotalSelected > 0 THEN
        PRINT #OutputFile, "FUNCTION vwatch64_CHECKWATCHPOINT"
        PRINT #OutputFile, "    DIM i AS LONG, DataType$"
        PRINT #OutputFile, "    GET #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTLISTBLOCK, vwatch64_WATCHPOINTLIST"
        PRINT #OutputFile, "    FOR i = 1 TO " + LTRIM$(STR$(TotalSelected))
        PRINT #OutputFile, "        IF ASC(vwatch64_WATCHPOINTLIST, i) = 1 THEN"
        PRINT #OutputFile, "            GET #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTEXPBLOCK, vwatch64_WATCHPOINT()"
        PRINT #OutputFile, "            DataType$ = UCASE$(RTRIM$(vwatch64_VARIABLES(i).DATATYPE))"
        PRINT #OutputFile, "            IF INSTR(DataType$, " + Q$ + "STRING" + Q$ + ") THEN DataType$ = " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "            IF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = " + Q$ + "=" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$ + ""
        PRINT #OutputFile, "                       IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) = RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                       IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) = VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN"
        PRINT #OutputFile, "                           GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = " + Q$ + "<=" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) <= RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) <= VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = " + Q$ + ">=" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) >= RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) >= VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = " + Q$ + "<>" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) <> RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) <> VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = " + Q$ + "<" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) < RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) < VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = " + Q$ + ">" + Q$ + " THEN"
        PRINT #OutputFile, "                SELECT CASE DataType$"
        PRINT #OutputFile, "                    CASE " + Q$ + "STRING" + Q$
        PRINT #OutputFile, "                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) > RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                    CASE ELSE"
        PRINT #OutputFile, "                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) > VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN"
        PRINT #OutputFile, "                            GOTO WatchpointStop"
        PRINT #OutputFile, "                        END IF"
        PRINT #OutputFile, "                END SELECT"
        PRINT #OutputFile, "            END IF"
        PRINT #OutputFile, "        END IF"
        PRINT #OutputFile, "    NEXT i"
        PRINT #OutputFile, ""
        PRINT #OutputFile, "    EXIT FUNCTION"
        PRINT #OutputFile, ""
        PRINT #OutputFile, "   WatchpointStop:"
        PRINT #OutputFile, "   vwatch64_WATCHPOINTCOMMAND.ACTION = vwatch64_NEXTSTEP"
        PRINT #OutputFile, "   vwatch64_WATCHPOINTCOMMAND.LINENUMBER = i"
        PRINT #OutputFile, "   PUT #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTCOMMANDBLOCK, vwatch64_WATCHPOINTCOMMAND"
        PRINT #OutputFile, "   vwatch64_CHECKWATCHPOINT = -1"
        PRINT #OutputFile, "END FUNCTION"
    END IF
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    PRINT #OutputFile, "'End of vWATCH64 procedures."
    PRINT #OutputFile, "'--------------------------------------------------------------------------------"
    CLOSE OutputFile
    ProcessStepDescription = "Output file generated."
    VerboseMaxProgress = 100
    VerboseProgress = 100
    GOSUB AddVerboseOutputLine

    $IF WIN THEN
        ThisPath$ = ""
        ExecutableExtension$ = ".exe"
    $ELSE
        ThisPath$ = "./"
        ExecutableExtension$ = ""
    $END IF
    Compiler$ = "qb64" + ExecutableExtension$

    IF NOT DONTCOMPILE AND _FILEEXISTS(Compiler$) THEN
        ProcessStepDescription = "Attempting to compile..."
        StatusMessage = Compiler$ + " -x" + IIFSTR$(SET_OPTION_EXPLICIT, " -e ", " ") + Q$ + NOPATH$(NEWFILENAME$) + Q$
        VerboseMaxProgress = 100
        VerboseProgress = 0
        GOSUB AddVerboseOutputLine
        AttemptCompile% = SHELL(ThisPath$ + Compiler$ + " -x" + IIFSTR$(SET_OPTION_EXPLICIT, " -e ", " ") + Q$ + NEWFILENAME$ + Q$)
        IF AttemptCompile% <> 0 THEN
            Message$ = ""
            Message$ = Message$ + "Compilation failed (error code: " + TRIM$(STR$(AttemptCompile%)) + ")." + CHR$(LF)
            Message$ = Message$ + "The file has been output, you will have to compile it yourself."
            PCOPY 0, 1
            MESSAGEBOX_RESULT = MESSAGEBOX("Compilation failed", Message$, MKI$(OK_ONLY), 1, 0)
            EXIT SUB
        ELSE
            ProcessStepDescription = "Compilation successful. Launching..."
            VerboseMaxProgress = 100
            VerboseProgress = 0
            GOSUB AddVerboseOutputLine
            IF _FILEEXISTS(LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$) THEN
                SHELL _DONTWAIT ThisPath$ + LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$
            ELSE
                Message$ = ""
                Message$ = Message$ + "Could not run " + LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$ + "."
                Message$ = Message$ + "The file has been output, you will have to compile/run it yourself."
                PCOPY 0, 1
                MESSAGEBOX_RESULT = MESSAGEBOX("Error", Message$, MKI$(OK_ONLY), 1, 0)
                EXIT SUB
            END IF
        END IF
    END IF

    EXIT SUB

    AddThisKeyword:
    TotalKeywords = TotalKeywords + 1
    REDIM _PRESERVE KeywordList(1 TO TotalKeywords) AS STRING
    KeywordList(TotalKeywords) = ThisKeyword
    RETURN

    AddOutputLine:
    TotalOutputLines = TotalOutputLines + 1
    REDIM _PRESERVE OutputLines(1 TO TotalOutputLines) AS STRING
    RETURN

    AddNextLineData:
    TotalNextLineData = TotalNextLineData + 1
    REDIM _PRESERVE SetNextLineData(1 TO TotalNextLineData) AS STRING
    SetNextLineData(TotalNextLineData) = "vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine))
    RETURN

    AddEndOfMainModuleCode:
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "IF vwatch64_HEADER.CONNECTED THEN"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    vwatch64_HEADER.CONNECTED = 0"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "END IF"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "CLOSE #vwatch64_CLIENTFILE"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "ON ERROR GOTO vwatch64_FILEERROR"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "KILL vwatch64_FILENAME"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ""
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "END"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_FILEERROR:"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "RESUME NEXT"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ""
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_CLIENTFILEERROR:"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "IF vwatch64_HEADER.CONNECTED THEN OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "RESUME"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ""
    RETURN

    AddSFVariableWatchCode:
    PRINT #OutputFile, "vwatch64_VARIABLEWATCH:"
    PRINT #OutputFile, "IF vwatch64_HEADER.CONNECTED = 0 THEN RETURN"
    PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
    tempindex.SFvar = 0
    FOR sf.Var = 1 TO TOTALVARIABLES
        IF ASC(AddedList$, sf.Var) = 1 THEN
            tempindex.SFvar = tempindex.SFvar + 1
            IF TRIM$(VARIABLES(sf.Var).SCOPE) = CurrentSubFunc$ OR TRIM$(VARIABLES(sf.Var).SCOPE) = "SHARED" THEN
                IF INSTR(VARIABLES(sf.Var).DATATYPE, "STRING") THEN
                    PRINT #OutputFile, "    vwatch64_VARIABLEDATA(" + LTRIM$(STR$(tempindex.SFvar)) + ").VALUE = " + TRIM$(VARIABLES(sf.Var).NAME)
                ELSE
                    PRINT #OutputFile, "    vwatch64_VARIABLEDATA(" + LTRIM$(STR$(tempindex.SFvar)) + ").VALUE = STR$(" + TRIM$(VARIABLES(sf.Var).NAME) + ")"
                END IF
            END IF
        END IF
    NEXT sf.Var
    PRINT #OutputFile, "ON ERROR GOTO 0"
    PRINT #OutputFile, "RETURN"
    PRINT #OutputFile, ""
    GOSUB AddSetVarCode
    RETURN

    AddSetVarCode:
    PRINT #OutputFile, ""
    PRINT #OutputFile, "vwatch64_SETVARIABLE:"
    PRINT #OutputFile, "ON ERROR GOTO vwatch64_CLIENTFILEERROR"
    PRINT #OutputFile, "GET #vwatch64_CLIENTFILE, vwatch64_EXCHANGEBLOCK, vwatch64_EXCHANGEDATASIZE$4"
    PRINT #OutputFile, "vwatch64_TARGETVARINDEX = CVL(vwatch64_EXCHANGEDATASIZE$4)"
    PRINT #OutputFile, "GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATASIZE$4"
    PRINT #OutputFile, "vwatch64_EXCHANGEDATA = SPACE$(CVL(vwatch64_EXCHANGEDATASIZE$4))"
    PRINT #OutputFile, "GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATA"
    PRINT #OutputFile, "vwatch64_BREAKPOINT.ACTION = vwatch64_READY"
    PRINT #OutputFile, "PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "SELECT CASE vwatch64_TARGETVARINDEX"
    tempindex.SFvar = 0
    FOR sf.Var = 1 TO TOTALVARIABLES
        IF ASC(AddedList$, sf.Var) = 1 THEN
            tempindex.SFvar = tempindex.SFvar + 1
            IF INSTR(TRIM$(VARIABLES(sf.Var).SCOPE), CurrentSubFunc$) > 0 OR TRIM$(VARIABLES(sf.Var).SCOPE) = "SHARED" THEN
                IF INSTR(VARIABLES(sf.Var).DATATYPE, "STRING") THEN
                    PRINT #OutputFile, "    CASE " + LTRIM$(STR$(tempindex.SFvar)) + ": " + TRIM$(VARIABLES(sf.Var).NAME) + " = vwatch64_EXCHANGEDATA"
                ELSE
                    DataType$ = UCASE$(TRIM$(VARIABLES(sf.Var).DATATYPE))
                    PRINT #OutputFile, "    CASE " + LTRIM$(STR$(tempindex.SFvar)) + ": " + TRIM$(VARIABLES(sf.Var).NAME) + " = _CV(" + DataType$ + ", vwatch64_EXCHANGEDATA)"
                END IF
            END IF
        END IF
    NEXT sf.Var
    PRINT #OutputFile, "END SELECT"
    IF CurrentSubFunc$ <> "MAIN MODULE" THEN
        PRINT #OutputFile, "GOSUB vwatch64_VARIABLEWATCH"
    END IF
    PRINT #OutputFile, "ON ERROR GOTO 0"
    PRINT #OutputFile, "RETURN"
    RETURN


    AddGotoNextLineCode:
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_SETNEXTLINE:"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "SELECT CASE vwatch64_NEXTLINE"
    FOR nextline.I = 1 TO TotalNextLineData
        GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "    CASE " + LTRIM$(STR$(VAL(RIGHT$(SetNextLineData(nextline.I), LEN(SetNextLineData(nextline.I)) - 15))))
        OutputLines(TotalOutputLines) = OutputLines(TotalOutputLines) + ": GOTO " + SetNextLineData(nextline.I)
    NEXT nextline.I
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "END SELECT"
    GOSUB AddOutputLine: OutputLines(TotalOutputLines) = ""
    RETURN

    AddSFParametersAsVariables:
    DO
        caseBkpNextVar$ = GETNEXTVARIABLE$(caseBkpSourceLine, -1)
        NextVar$ = UCASE$(caseBkpNextVar$)
        IF NextVar$ = "" THEN EXIT DO
        IF TRIM$(NextVar$) = "STATIC" THEN GOTO StaticIsNotAParameter

        IF INSTR(NextVar$, " AS ") = 0 THEN
            'Attempt to infer DATA TYPE from suffixes:
            FoundType = SUFFIXLOOKUP$(NextVar$)
            DefaultTypeUsed = 0

            IF LEN(FoundType) = 0 THEN
                FoundType = DEFAULTDATATYPE(ASC(NextVar$, 1)) 'Assume default data type
                DefaultTypeUsed = -1
            END IF

            IsArray = 0
            IF INSTR(NextVar$, "(") THEN IsArray = -1
            IF IsArray THEN
                'Arrays as parameters are ignored, as they will have already been defined elsewhere.
            ELSE
                TOTALVARIABLES = TOTALVARIABLES + 1
                REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                VARIABLES(TOTALVARIABLES).NAME = caseBkpNextVar$
                VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
            END IF
        ELSE
            FoundType = RIGHT$(NextVar$, LEN(NextVar$) - INSTR(NextVar$, " AS ") - 3)

            IF CHECKLIST(FoundType, KeywordList(), INTERNALKEYWORDS) THEN
                'Variable is defined as an internal DATA TYPE.
                IsArray = 0
                IF INSTR(NextVar$, "(") THEN IsArray = -1
                IF IsArray THEN
                    'Arrays as parameters are ignored, as they will have already been defined elsewhere.
                ELSE
                    TOTALVARIABLES = TOTALVARIABLES + 1
                    REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                    VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1)
                    VARIABLES(TOTALVARIABLES).SCOPE = CurrentSubFunc$
                    VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                END IF
            ELSE
                'Variable is defined as a user defined type.
                'UDTs as parameters are ignored, as they will have already been defined elsewhere.
            END IF
        END IF
        StaticIsNotAParameter:
    LOOP
    RETURN

    AddVerboseOutputLine:
    IF PrevStepDescription$ <> ProcessStepDescription THEN
        PrevStepDescription$ = ProcessStepDescription
        IF PrevMessage$ = StatusMessage THEN
            StatusMessage = ""
        END IF
    END IF
    Message$ = ""
    Message$ = Message$ + ProcessStepDescription + CHR$(LF)
    Message$ = Message$ + StatusMessage
    PROGRESSBOX "Processing...", Message$, VerboseMaxProgress, VerboseProgress
    RETURN

    AddThisTempVar:
    'Add temporary variable to watchlist: -------------------------
    IF MainModule THEN ThisTempScope$ = "MAIN MODULE" ELSE ThisTempScope$ = TRIM$(SUBFUNC(CurrSF).NAME)
    TempList$ = STRING$(TOTALVARIABLES, 1)
    StartAt = 0
    LookAgain:
    StartAt = StartAt + 1
    Found = FINDVARIABLES(StartAt, NextVar$, TempList$)
    IF Found = 0 THEN
        'Before assuming the variable doesn't exist, check against ExpandedWithUDT()
        FOR CheckExpanded = 1 TO TotalExpandedWithUDT
            IF UCASE$(TRIM$(ExpandedWithUDT(CheckExpanded).NAME)) = UCASE$(NextVar$) THEN
                IF UCASE$(TRIM$(ExpandedWithUDT(CheckExpanded).SCOPE)) = ThisTempScope$ OR _
                    UCASE$(TRIM$(ExpandedWithUDT(CheckExpanded).SCOPE)) = "SHARED" THEN
                    GOTO NoValidVarFound
                END IF
            END IF
        NEXT

        IF CurrSF > 0 THEN
            Found = FIND_KEYWORD(TRIM$(SUBFUNC(CurrSF).NAME), NextVar$, FoundAt)
            IF Found THEN GOTO NoValidVarFound
        END IF
        'Attempt to infer DATA TYPE from suffixes:
        FoundType = SUFFIXLOOKUP$(NextVar$)
        DefaultTypeUsed = 0

        IF LEN(FoundType) = 0 THEN
            FoundType = DEFAULTDATATYPE(ASC(NextVar$, 1)) 'Assume default data type
            DefaultTypeUsed = -1
        END IF

        TOTALVARIABLES = TOTALVARIABLES + 1
        REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
        VARIABLES(TOTALVARIABLES).NAME = caseBkpNextVar$
        IF MainModule THEN
            VARIABLES(TOTALVARIABLES).SCOPE = "MAIN MODULE"
        ELSE
            VARIABLES(TOTALVARIABLES).SCOPE = TRIM$(SUBFUNC(CurrSF).NAME)
        END IF
        VARIABLES(TOTALVARIABLES).DATATYPE = FoundType

        IF MainModule THEN
            TotalLocalVariables = TotalLocalVariables + 1
            REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
            LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
            LOCALVARIABLES(TotalLocalVariables).DATATYPE = IIFSTR$(DefaultTypeUsed, "", VARIABLES(TOTALVARIABLES).DATATYPE)
        END IF
    ELSE
        'Check if scope is the same; if not, we can add this var.
        IF MainModule THEN
            IF TRIM$(VARIABLES(Found).SCOPE) = "MAIN MODULE" OR TRIM$(VARIABLES(Found).SCOPE) = "SHARED" THEN
                'Can't add this var again.
            ELSE
                StartAt = Found: GOTO LookAgain
            END IF
        ELSE
            IF TRIM$(VARIABLES(Found).SCOPE) = TRIM$(SUBFUNC(CurrSF).NAME) OR TRIM$(VARIABLES(Found).SCOPE) = "SHARED" THEN
                'Can't add this var again.
            ELSE
                StartAt = Found: GOTO LookAgain
            END IF
        END IF
    END IF
    '--------------------------------------------------------------
    RETURN
END SUB

'------------------------------------------------------------------------------
FUNCTION CHECKLIST (Text$, List$(), UpperBoundary%)
    'Checks if Text$ is in List$()
    FOR i = 1 TO UpperBoundary%
        IF TRIM$(List$(i)) = TRIM$(Text$) THEN
            CHECKLIST = i
        END IF
    NEXT i
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION FINDVARIABLES (StartAt, Text$, AddedList$)
    FOR i = StartAt TO TOTALVARIABLES
        IF UCASE$(TRIM$(VARIABLES(i).NAME)) = UCASE$(TRIM$(Text$)) AND ASC(AddedList$, i) = 1 THEN
            FINDVARIABLES = i
            EXIT FUNCTION
        END IF
    NEXT i
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION STRIPCOMMENTS$ (Text AS STRING)
    DIM OpenQuotation AS _BYTE
    DIM TextRebuilt AS STRING
    DIM i AS INTEGER

    IF LEFT$(Text, 1) = "'" THEN EXIT FUNCTION
    IF LEFT$(Text, 4) = "REM " THEN EXIT FUNCTION

    FOR i = 1 TO LEN(Text)
        IF i > 1 AND (UCASE$(MID$(Text, i, 5)) = " REM " OR UCASE$(MID$(Text, i, 5)) = ":REM ") AND OpenQuotation = 0 THEN
            STRIPCOMMENTS$ = TextRebuilt
            EXIT FUNCTION
        END IF
        SELECT CASE MID$(Text, i, 1)
            CASE "'"
                IF NOT OpenQuotation THEN
                    'Found a comment. This is the end of parsing.
                    STRIPCOMMENTS$ = TextRebuilt
                    EXIT FUNCTION
                END IF
            CASE CHR$(34) 'Quotation marks
                OpenQuotation = NOT OpenQuotation
        END SELECT
        TextRebuilt = TextRebuilt + MID$(Text, i, 1)
    NEXT i
    STRIPCOMMENTS$ = TextRebuilt
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION NOPATH$ (FILENAME$)
    div$ = "/\"
    IF INSTR(FILENAME$, LEFT$(div$, 1)) = 0 AND INSTR(FILENAME$, RIGHT$(div$, 1)) = 0 THEN NOPATH$ = FILENAME$: EXIT FUNCTION
    FOR i = LEN(FILENAME$) TO 1 STEP -1
        c$ = MID$(FILENAME$, i, 1)
        IF INSTR(div$, c$) > 0 THEN
            NOPATH$ = RIGHT$(FILENAME$, LEN(FILENAME$) - i)
            EXIT FUNCTION
        END IF
    NEXT
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION PATHONLY$ (FILENAME$)
    PATHONLY$ = LEFT$(FILENAME$, LEN(FILENAME$) - LEN(NOPATH$(FILENAME$)))
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION TRIM$ (Text$)
    TRIM$ = RTRIM$(LTRIM$(TRUNCATE$(Text$, 0)))
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION TRUNCATE$ (Text$, Char)
    i = INSTR(Text$, CHR$(Char))
    IF i > 0 THEN TRUNCATE$ = LEFT$(Text$, i - 1) ELSE TRUNCATE$ = Text$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION REMOVESIGIL$ (Var AS STRING)
    IsUnsigned = INSTR(Var, "~")
    IF IsUnsigned > 0 THEN Temp$ = LEFT$(Var, IsUnsigned - 1): GOTO Done

    ThisVarSuffix$ = SUFFIXLOOKUP(Var)
    IF ThisVarSuffix$ = "" THEN Temp$ = Var: GOTO Done
    SELECT CASE ThisVarSuffix$
        CASE "INTEGER", "LONG", "SINGLE", "DOUBLE"
            Temp$ = LEFT$(Var, LEN(Var) - 1)
        CASE "STRING"
            Temp$ = LEFT$(Var, INSTR(Var, "$") - 1)
        CASE "_BIT"
            Temp$ = LEFT$(Var, INSTR(Var, "`") - 1)
        CASE "_BYTE", "_INTEGER64", "_FLOAT"
            Temp$ = LEFT$(Var, LEN(Var) - 2)
    END SELECT
    Done:
    REMOVESIGIL$ = Temp$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION SUFFIXLOOKUP$ (Var AS STRING)
    DIM VarBKP AS STRING

    VarBKP = Var
    IF LEN(VarBKP) < 2 THEN EXIT FUNCTION

    IF INSTR(VarBKP, "(") > 0 THEN 'An array was found. Let's strip it of its brackets.
        VarBKP = LEFT$(VarBKP, INSTR(VarBKP, "(") - 1)
    END IF

    SELECT CASE MID$(VarBKP, LEN(VarBKP), 1)
        CASE "$"
            SUFFIXLOOKUP$ = "STRING"
        CASE "`"
            SUFFIXLOOKUP$ = "_BIT"
        CASE "%"
            SUFFIXLOOKUP$ = "INTEGER"
        CASE "&"
            SUFFIXLOOKUP$ = "LONG"
        CASE "!"
            SUFFIXLOOKUP$ = "SINGLE"
        CASE "#"
            SUFFIXLOOKUP$ = "DOUBLE"
        CASE "0" TO "9"
            FOR i = LEN(VarBKP) - 1 TO 1 STEP -1
                SELECT CASE MID$(VarBKP, i, 1)
                    CASE "0" TO "9"
                        'Numbers allowed. Won't check for validity. Leave that to qb64 compiler.
                    CASE "`"
                        SUFFIXLOOKUP$ = "_BIT"
                        EXIT FUNCTION
                    CASE "$"
                        SUFFIXLOOKUP$ = "STRING"
                        EXIT FUNCTION
                    CASE ELSE
                        EXIT FUNCTION
                END SELECT
            NEXT i
    END SELECT

    IF LEN(VarBKP) < 3 THEN EXIT FUNCTION 'no more suffixes to evaluate

    SELECT CASE MID$(VarBKP, LEN(VarBKP) - 1, 1)
        CASE "~"
            SUFFIXLOOKUP$ = "_UNSIGNED " + SUFFIXLOOKUP$
        CASE "%"
            SUFFIXLOOKUP$ = "_BYTE"
        CASE "&"
            SUFFIXLOOKUP$ = "_INTEGER64"
        CASE "#"
            SUFFIXLOOKUP$ = "_FLOAT"
    END SELECT

    IF LEN(VarBKP) < 4 THEN EXIT FUNCTION 'no more suffixes to evaluate
    IF MID$(VarBKP, LEN(VarBKP) - 2, 1) = "~" THEN SUFFIXLOOKUP$ = "_UNSIGNED " + SUFFIXLOOKUP$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION IIF (Condition, IfTrue, IfFalse)
    IF Condition THEN IIF = IfTrue ELSE IIF = IfFalse
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION IIFSTR$ (Condition, IfTrue$, IfFalse$)
    IF Condition THEN IIFSTR$ = IfTrue$ ELSE IIFSTR$ = IfFalse$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION GETNEXTVARIABLE$ (Text$, WhichLine)
    'Parses a line of code in which more than one variable
    'may have been defined using commas. Returns an empty
    'string if there are no more variables in the line.

    DIM InBrackets AS INTEGER
    STATIC LastLine AS LONG
    STATIC LastSF$
    STATIC EndOfStatement AS _BYTE

    IF WhichLine = -2 THEN 'Reset STATIC variables
        LastSF$ = ""
        LastLine = 0
        POSITION_IN_LINE = 0
        EndOfStatement = 0
        EXIT FUNCTION
    END IF

    IF LEN(Text$) = 0 THEN EXIT FUNCTION

    Result$ = ""
    IF EndOfStatement THEN
        EndOfStatement = 0
        GETNEXTVARIABLE$ = ""
        EXIT FUNCTION
    END IF

    IF (WhichLine > 0) AND (WhichLine <> LastLine) THEN
        'First time this line is passed
        POSITION_IN_LINE = 1
        LastLine = WhichLine

        IF UCASE$(LEFT$(Text$, 4)) = "DIM " THEN
            POSITION_IN_LINE = 4
            IF MID$(Text$, 5, 7) = "SHARED " THEN POSITION_IN_LINE = 11
        ELSEIF UCASE$(LEFT$(Text$, 7)) = "COMMON " THEN
            POSITION_IN_LINE = 7
            IF MID$(Text$, 8, 7) = "SHARED " THEN POSITION_IN_LINE = 14
        ELSEIF UCASE$(LEFT$(Text$, 7)) = "STATIC " THEN
            POSITION_IN_LINE = 7
        END IF
    ELSEIF (WhichLine = -1) THEN
        'Process SUB/FUNCTION parameters instead of DIM variables
        IF LastSF$ <> Text$ THEN
            LastSF$ = Text$
            POSITION_IN_LINE = INSTR(Text$, "(")
        END IF
    END IF

    IF WhichLine > 0 THEN
        DO
            POSITION_IN_LINE = POSITION_IN_LINE + 1
            IF POSITION_IN_LINE > LEN(Text$) THEN EXIT DO
            Char$ = MID$(Text$, POSITION_IN_LINE, 1)
            SELECT CASE Char$
                CASE "(": InBrackets = InBrackets + 1
                CASE ")": InBrackets = InBrackets - 1
                CASE ",": IF InBrackets = 0 THEN EXIT DO
                CASE ":"
                    IF INSTR(POSITION_IN_LINE, UCASE$(Text$), "DIM SHARED ") > 0 THEN
                        POSITION_IN_LINE = INSTR(POSITION_IN_LINE, UCASE$(Text$), "DIM SHARED ") + 10
                    ELSEIF INSTR(POSITION_IN_LINE, UCASE$(Text$), "DIM ") > 0 THEN
                        POSITION_IN_LINE = INSTR(POSITION_IN_LINE, UCASE$(Text$), "DIM ") + 3
                    ELSEIF INSTR(POSITION_IN_LINE, UCASE$(Text$), "COMMON SHARED ") > 0 THEN
                        POSITION_IN_LINE = INSTR(POSITION_IN_LINE, UCASE$(Text$), "COMMON SHARED ") + 13
                    ELSEIF INSTR(POSITION_IN_LINE, UCASE$(Text$), "COMMON ") > 0 THEN
                        POSITION_IN_LINE = INSTR(POSITION_IN_LINE, UCASE$(Text$), "COMMON ") + 6
                    ELSEIF INSTR(POSITION_IN_LINE, UCASE$(Text$), "STATIC ") > 0 THEN
                        POSITION_IN_LINE = INSTR(POSITION_IN_LINE, UCASE$(Text$), "STATIC ") + 6
                    ELSE
                        EndOfStatement = -1
                    END IF
                    EXIT DO
                CASE "_": IF POSITION_IN_LINE = LEN(Text$) THEN EXIT DO
            END SELECT
            Result$ = Result$ + Char$
        LOOP
    ELSEIF WhichLine = -1 THEN
        DO
            POSITION_IN_LINE = POSITION_IN_LINE + 1
            IF POSITION_IN_LINE > LEN(Text$) THEN EXIT DO
            Char$ = MID$(Text$, POSITION_IN_LINE, 1)
            SELECT CASE Char$
                CASE "(": InBrackets = InBrackets + 1
                CASE ")": InBrackets = InBrackets - 1: IF InBrackets = -1 THEN EXIT DO
                CASE ",": EXIT DO
                CASE ":": EndOfStatement = -1: EXIT DO
            END SELECT
            Result$ = Result$ + Char$
        LOOP
    END IF

    GETNEXTVARIABLE$ = TRIM$(Result$)
END FUNCTION


'------------------------------------------------------------------------------
SUB SETUP_CONNECTION
    DIM InsideCheckingOffBlock AS _BYTE
    DIM TotalSourceLines AS LONG

    _KEYCLEAR 'Clears the keyboard buffer
    _ACCEPTFILEDROP

    TotalButtons = 2
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE
    b = 1
    Buttons(b).CAPTION = "<Open and Process .BAS>": b = b + 1
    Buttons(b).CAPTION = "<ESC=Exit>": b = b + 1

    StartSetup:
    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)

    'Initialize variables:
    HEADER.CONNECTED = 0
    HEADER.CLIENT_ID = ""
    HEADER.VERSION = ""
    HEADER.RESPONSE = 0

    CLIENT.NAME = ""
    CLIENT.CHECKSUM = ""
    CLIENT.TOTALSOURCELINES = 0
    CLIENT.EXENAME = ""
    CLIENT.LINENUMBER = 0
    CLIENT.TOTALVARIABLES = 0

    CLOSE #FILE
    FILE = FREEFILE
    ON ERROR GOTO FileError
    'Try killing vwatch64.dat. Won't work if open ([WINDOWS]), so we'll try to reconnect to client.
    IF _FILEEXISTS(_CWD$ + PATHSEP$ + "vwatch64.dat") THEN KILL _CWD$ + PATHSEP$ + "vwatch64.dat"
    ON ERROR GOTO 0

    'Opens "vwatch64.dat" to wait for a connection:
    OPEN _CWD$ + PATHSEP$ + "vwatch64.dat" FOR BINARY AS #FILE

    HEADERBLOCK = 1
    CLIENTBLOCK = LEN(HEADER) + 1
    BREAKPOINTBLOCK = CLIENTBLOCK + LEN(CLIENT) + 1
    BREAKPOINTLISTBLOCK = BREAKPOINTBLOCK + LEN(BREAKPOINT) + 1

    'Wait for a connection:
    x = _EXIT
    MENU% = 0
    DO: _LIMIT 30
        GET #FILE, HEADERBLOCK, HEADER
        GOSUB GetInput
        IF k = 27 THEN USERQUIT = -1
        IF (k = 111 OR k = 79) AND ctrlDown = -1 THEN MENU% = 101
        IF MENU% = 101 THEN CLOSE #FILE: _ACCEPTFILEDROP OFF: EXIT SUB
        IF _TOTALDROPPEDFILES > 0 THEN EXIT SUB
        IF _EXIT THEN USERQUIT = -1
        GOSUB UpdateScreen
    LOOP UNTIL USERQUIT OR HEADER.CONNECTED = -1 OR MENU% = 102

    _ACCEPTFILEDROP OFF

    IF USERQUIT OR MENU% = 102 THEN
        CLOSE #FILE
        ON ERROR GOTO FileError
        KILL _CWD$ + PATHSEP$ + "vwatch64.dat"
        ON ERROR GOTO 0
        SYSTEM
    END IF

    CLS , _RGB32(255, 255, 255)
    'Connected! Check if client is compatible:
    IF HEADER.CLIENT_ID <> ID OR HEADER.VERSION <> VERSION THEN
        Message$ = ""
        Message$ = Message$ + "Attempted connection by client with ID " + CHR$(34) + HEADER.CLIENT_ID + CHR$(34) + CHR$(LF)
        Message$ = Message$ + "Reported version: " + HEADER.VERSION
        MESSAGEBOX_RESULT = MESSAGEBOX("Client not compatible", Message$, MKI$(OK_ONLY), 1, 0)
        HEADER.CONNECTED = 0
        PUT #FILE, HEADERBLOCK, HEADER
        GOTO StartSetup
    END IF

    'Send autorization to client:
    HEADER.RESPONSE = -1
    PUT #FILE, HEADERBLOCK, HEADER

    CLS
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Connected. Waiting for client's ID..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$


    'Wait for data to be sent by client:
    Start# = TIMER
    DO: _LIMIT 30
        GET #FILE, CLIENTBLOCK, CLIENT
        IF TIMER - Start# > TIMEOUTLIMIT THEN EXIT DO
    LOOP UNTIL LEN(TRIM$(CLIENT.CHECKSUM)) > 0

    'No CHECKSUM received = connection closed.
    IF LEN(TRIM$(CLIENT.CHECKSUM)) = 0 THEN SYSTEM_BEEP 0: GOTO StartSetup

    REDIM VARIABLES(1 TO CLIENT.TOTALVARIABLES) AS VARIABLESTYPE
    REDIM VARIABLE_DATA(1 TO CLIENT.TOTALVARIABLES) AS VARIABLEVALUETYPE
    REDIM WATCHPOINT(1 TO CLIENT.TOTALVARIABLES) AS WATCHPOINTTYPE
    TOTALBREAKPOINTS = 0
    BREAKPOINTLIST = STRING$(CLIENT.TOTALSOURCELINES, 0)
    WATCHPOINTLIST = STRING$(CLIENT.TOTALVARIABLES, 0)
    IF CLIENT.TOTALVARIABLES <= 15 THEN
        'Up to 15 variables will be added automatically to quick watch panel
        SELECTED_VARIABLES = STRING$(CLIENT.TOTALVARIABLES, 1)
        TOTAL_SELECTEDVARIABLES = CLIENT.TOTALVARIABLES
    ELSE
        SELECTED_VARIABLES = STRING$(CLIENT.TOTALVARIABLES, 0)
        TOTAL_SELECTEDVARIABLES = 0
    END IF
    DATAINFOBLOCK = BREAKPOINTLISTBLOCK + LEN(BREAKPOINTLIST) + 1
    GET #FILE, DATAINFOBLOCK, VARIABLES()
    DATABLOCK = DATAINFOBLOCK + LEN(VARIABLES()) + 1
    WATCHPOINTLISTBLOCK = DATABLOCK + LEN(VARIABLE_DATA()) + 1
    WATCHPOINTEXPBLOCK = WATCHPOINTLISTBLOCK + LEN(WATCHPOINTLIST) + 1
    WATCHPOINTCOMMANDBLOCK = WATCHPOINTEXPBLOCK + LEN(WATCHPOINT()) + 1
    EXCHANGEBLOCK = WATCHPOINTCOMMANDBLOCK + LEN(WATCHPOINT_COMMAND) + 1
    LINE_TRAIL = LEN("[ ]  " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN("1")) + "1" + "    ")

    LONGESTSCOPESPEC = 1
    LONGESTVARNAME = 1
    FOR i = 1 TO CLIENT.TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > LONGESTVARNAME THEN LONGESTVARNAME = LEN(TRIM$(VARIABLES(i).NAME))
        IF LEN(TRIM$(VARIABLES(i).SCOPE)) > LONGESTSCOPESPEC THEN LONGESTSCOPESPEC = LEN(TRIM$(VARIABLES(i).SCOPE))
        IF ASC(WATCHPOINTLIST, i) = 1 THEN TOTALWATCHPOINTS = TOTALWATCHPOINTS + 1
    NEXT i

    'Load the source file, if it still exists.
    SOURCEFILE = ""
    IF _FILEEXISTS(TRIM$(CLIENT.NAME)) THEN
        FILENAME$ = TRIM$(CLIENT.NAME)
        MergeResult = OpenInclude(FILENAME$, SOURCECODE(), TotalSourceLines)
        IF MergeResult = MISSINGFILE THEN
            IF CLIENT.TOTALVARIABLES > 0 THEN
                Message$ = ""
                Message$ = Message$ + "One of the $INCLUDE files could not be found" + CHR$(LF)
                Message$ = Message$ + "('" + NOPATH$(FILENAME$) + "' on line" + STR$(TotalSourceLines) + ")." + CHR$(LF)
                Message$ = Message$ + "Source view will be empty. Continue?"
                IF MESSAGEBOX("File not found", Message$, MKI$(YN_QUESTION), 1, -1) = MB_NO THEN
                    HEADER.CONNECTED = 0
                    PUT #FILE, HEADERBLOCK, HEADER
                    GOTO StartSetup
                ELSE
                    GOTO SkipSourceScan
                END IF
            ELSE
                SOURCEFILE = ""
                HEADER.CONNECTED = 0
                PUT #FILE, HEADERBLOCK, HEADER
                Message$ = ""
                Message$ = Message$ + "Some original files could not be found" + CHR$(LF)
                Message$ = Message$ + "and there are no watchable variables."
                MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MKI$(OK_ONLY), 1, 0)
                GOTO StartSetup
            END IF
        ELSE
        END IF

        FOR i = 1 TO TotalSourceLines
            SOURCEFILE = SOURCEFILE + SOURCECODE(i)
        NEXT i

        IF CLIENT.CHECKSUM <> ADLER32(SOURCEFILE, 0) THEN
            IF CLIENT.TOTALVARIABLES > 0 THEN
                SOURCEFILE = ""
                Message$ = ""
                Message$ = Message$ + "The original source files were changed." + CHR$(LF)
                Message$ = Message$ + "Source view will be empty (you can still watch variables)." + CHR$(LF)
                Message$ = Message$ + "Continue?"
                IF MESSAGEBOX("Checksum error", Message$, MKI$(YN_QUESTION), 1, -1) = MB_NO THEN
                    HEADER.CONNECTED = 0
                    PUT #FILE, HEADERBLOCK, HEADER
                    GOTO StartSetup
                END IF
            ELSE
                SOURCEFILE = ""
                HEADER.CONNECTED = 0
                PUT #FILE, HEADERBLOCK, HEADER
                Message$ = ""
                Message$ = Message$ + "The original source files were changed" + CHR$(LF)
                Message$ = Message$ + "and there are no watchable variables."
                MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MKI$(OK_ONLY), 1, 0)
                GOTO StartSetup
            END IF
        ELSE
            'Scan for $CHECKING/VWATCH64:OFF blocks and build SUB/FUNCTION list
            CHECKINGOFF_LINES = STRING$(CLIENT.TOTALSOURCELINES, 0)
            SOURCEFILE = "LOADED"
            REDIM SOURCECODE_COLORIZED(1 TO TotalSourceLines) AS _BYTE
            REDIM SUBFUNC(0) AS SUBFUNC_TYPE

            CurrentLineNo = 1
            LONGESTLINE = 1
            InsideCheckingOffBlock = 0
            DeclaringLibrary = 0
            TotalSubFunc = 0
            FOR i = 1 TO TotalSourceLines
                bkpSourceLine$ = SOURCECODE(i)
                caseBkpSourceLine$ = TRIM$(bkpSourceLine$)
                SourceLine$ = UCASE$(TRIM$(caseBkpSourceLine$))
                IF GETELEMENT$(SourceLine$, 1) = "STOP" THEN ASC(BREAKPOINTLIST, i) = 1: TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                IF SourceLine$ = "$CHECKING:OFF" OR LEFT$(SourceLine$, 13) = "'VWATCH64:OFF" THEN
                    InsideCheckingOffBlock = -1
                END IF
                IF InsideCheckingOffBlock THEN ASC(CHECKINGOFF_LINES, i) = 1
                IF SourceLine$ = "$CHECKING:ON" OR LEFT$(SourceLine$, 12) = "'VWATCH64:ON" THEN
                    InsideCheckingOffBlock = 0
                END IF
                IF LEN(bkpSourceLine$) > LONGESTLINE THEN LONGESTLINE = LEN(bkpSourceLine$)
                IF GETELEMENT$(SourceLine$, 1) = "DECLARE" AND FIND_SYMBOL(1, SourceLine$, "LIBRARY") THEN
                    DeclaringLibrary = -1
                ELSEIF GETELEMENT$(SourceLine$, 1) = "END" AND GETELEMENT$(SourceLine$, 2) = "DECLARE" THEN
                    DeclaringLibrary = 0
                END IF

                'SUB/FUNCTION LIST:--------------------------------------------------------------
                SourceLine$ = STRIPCOMMENTS$(SourceLine$)
                IF LEFT$(SourceLine$, 4) = "SUB " THEN
                    IF INSTR(SourceLine$, "(") THEN
                        CurrentSubFunc$ = TRIM$(MID$(caseBkpSourceLine$, 5, INSTR(SourceLine$, "(") - 5))
                    ELSE
                        CurrentSubFunc$ = MID$(caseBkpSourceLine$, 5)
                    END IF
                    TotalSubFunc = TotalSubFunc + 1
                    REDIM _PRESERVE SUBFUNC(1 TO TotalSubFunc) AS SUBFUNC_TYPE
                    SUBFUNC(TotalSubFunc).NAME = CurrentSubFunc$
                    SUBFUNC(TotalSubFunc).LINE = i
                    IF DeclaringLibrary THEN SUBFUNC(TotalSubFunc).EXTERNAL = -1
                ELSEIF LEFT$(SourceLine$, 9) = "FUNCTION " THEN
                    IF INSTR(SourceLine$, "(") THEN
                        CurrentSubFunc$ = TRIM$(MID$(caseBkpSourceLine$, 10, INSTR(SourceLine$, "(") - 10))
                    ELSE
                        CurrentSubFunc$ = MID$(caseBkpSourceLine$, 10)
                    END IF
                    TotalSubFunc = TotalSubFunc + 1
                    REDIM _PRESERVE SUBFUNC(1 TO TotalSubFunc) AS SUBFUNC_TYPE
                    SUBFUNC(TotalSubFunc).NAME = CurrentSubFunc$
                    SUBFUNC(TotalSubFunc).LINE = i
                    IF DeclaringLibrary THEN SUBFUNC(TotalSubFunc).EXTERNAL = -1
                ELSEIF LEFT$(SourceLine$, 7) = "END SUB" OR LEFT$(SourceLine$, 12) = "END FUNCTION" THEN
                    SUBFUNC(TotalSubFunc).ENDING = i
                    CurrentSubFunc$ = ""
                END IF
                'END OF SUB/FUNCTION LIST BUILDING-----------------------------------------------
            NEXT i
        END IF
    ELSE
        IF CLIENT.TOTALVARIABLES > 0 THEN
            SOURCEFILE = ""
            Message$ = ""
            Message$ = Message$ + "The original source file could not be found." + CHR$(LF)
            Message$ = Message$ + "Source view will be empty (you can still watch variables)." + CHR$(LF)
            Message$ = Message$ + "Continue?"
            IF MESSAGEBOX("Source not available", Message$, MKI$(YN_QUESTION), 1, -1) = MB_NO THEN
                HEADER.CONNECTED = 0
                PUT #FILE, HEADERBLOCK, HEADER
                GOTO StartSetup
            END IF
        ELSE
            SOURCEFILE = ""
            HEADER.CONNECTED = 0
            PUT #FILE, HEADERBLOCK, HEADER
            Message$ = ""
            Message$ = Message$ + "The original source file could not be found" + CHR$(LF)
            Message$ = Message$ + "and there are no watchable variables."
            MESSAGEBOX_RESULT = MESSAGEBOX(ID, Message$, MKI$(OK_ONLY), 1, 0)
            GOTO StartSetup
        END IF
    END IF

    SkipSourceScan:
    TITLESTRING = TITLESTRING + " - " + NOPATH$(TRIM$(CLIENT.NAME)) + " (PID: " + TRIM$(STR$(CLIENT.PID)) + IIFSTR$(LEN(TRIM$(CLIENT.EXENAME)), " - " + NOPATH$(TRIM$(CLIENT.EXENAME)) + ")", ")")
    _TITLE TITLESTRING

    'Connection estabilished.
    EXIT SUB

    GetInput:
    k$ = INKEY$
    modKey = _KEYHIT: k = modKey
    IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
    IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0
    DO: _LIMIT 500
        mx = _MOUSEX
        my = _MOUSEY
        mb = _MOUSEBUTTON(1)
    LOOP WHILE _MOUSEINPUT
    RETURN

    UpdateScreen:
    CLS , _RGB32(255, 255, 255)
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 5), _RGB32(0, 178, 179), BF

    _PUTIMAGE (_WIDTH - _WIDTH(WASP) - 15, _HEIGHT - 1 - _HEIGHT(WASP)), WASP

    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Waiting for a connection..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$
    t$ = "Launch the program that will be monitored now"
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT - _FONTHEIGHT * 2), t$
    t$ = "Ctrl+O to open and process a file / ESC to quit"
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT - _FONTHEIGHT), t$


    'Top buttons:
    ButtonLine$ = ""
    FOR cb = 1 TO TotalButtons
        c$ = TRIM$(Buttons(cb).CAPTION)
        ButtonLine$ = ButtonLine$ + IIFSTR$(LEN(c$), c$ + " ", "")
    NEXT cb

    FOR cb = 1 TO TotalButtons
        Buttons(cb).X = INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION)) * _FONTWIDTH + _PRINTWIDTH(ModeTitle$)
        Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
    NEXT cb

    GOSUB CheckButtons

    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$
    FOR i = 1 TO LEN(ButtonLine$)
        IF (ASC(ButtonLine$, i) <> 60) AND (ASC(ButtonLine$, i) <> 62) THEN
            ASC(ButtonLine$, i) = 32
        END IF
    NEXT i
    COLOR _RGB32(255, 255, 0)
    _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 2), ButtonLine$
    COLOR _RGB32(0, 0, 0)

    _DISPLAY
    RETURN

    CheckButtons:
    IF my > _FONTHEIGHT THEN RETURN
    'Hover highlight:
    FOR cb = 1 TO TotalButtons
        IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
            LINE (Buttons(cb).X - 3, 3)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
        END IF
    NEXT cb

    IF mb THEN
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                WHILE _MOUSEBUTTON(1): _LIMIT 500: mb = _MOUSEINPUT: WEND
                mb = 0: mx = _MOUSEX: my = _MOUSEY
                'Check if the user moved the mouse out of the button before releasing it (=cancel)
                IF my > _FONTHEIGHT THEN RETURN
                IF (mx < Buttons(cb).X) OR (mx > Buttons(cb).X + Buttons(cb).W) THEN RETURN
                IF INSTR(Buttons(cb).CAPTION, ".BAS") THEN MENU% = 101: RETURN
                IF INSTR(Buttons(cb).CAPTION, "ESC=") THEN MENU% = 102: RETURN
                SYSTEM_BEEP 0 'in case a button was added but not yet assigned
                RETURN
            END IF
        NEXT cb
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
SUB PARSEARRAY (ArrayName$, Valid%, LowerBoundary%, UpperBoundary%)
    DIM Position AS INTEGER
    DIM Char AS STRING
    DIM LowerBoundary$
    DIM UpperBoundary$

    Valid% = 0
    IF INSTR(ArrayName$, "(") = 0 OR INSTR(ArrayName$, ")") = 0 THEN EXIT SUB
    IF INSTR(ArrayName$, ",") THEN EXIT SUB

    Position = INSTR(ArrayName$, "(") + 1
    IF Position > LEN(ArrayName$) THEN EXIT SUB

    'Read lower boundary
    FOR i = Position TO LEN(ArrayName$)
        Char = MID$(ArrayName$, i, 1)
        IF ASC(Char) >= 48 AND ASC(Char) <= 57 THEN
            LowerBoundary$ = LowerBoundary$ + Char
        ELSEIF Char = ")" THEN
            'If bracket was closed, then we only have an upper boundary.
            LowerBoundary% = SET_OPTIONBASE
            UpperBoundary% = VAL(LowerBoundary$)
            Valid% = -1
            EXIT SUB
        ELSE
            'Lower boundary found.
            EXIT FOR
        END IF
    NEXT i
    LowerBoundary% = VAL(LowerBoundary$)
    Position = i

    'Read upper boundary
    FOR i = Position TO LEN(ArrayName$)
        Char = MID$(ArrayName$, i, 1)
        IF ASC(Char) >= 48 AND ASC(Char) <= 57 THEN
            UpperBoundary$ = UpperBoundary$ + Char
        ELSEIF Char = ")" THEN
            'If bracket was closed, our job is done
            UpperBoundary% = VAL(UpperBoundary$)
            Valid% = -1
            EXIT SUB
        END IF
    NEXT i
END SUB

'------------------------------------------------------------------------------
FUNCTION MULTI_SEARCH (FullText$, SearchString$)
    'Returns -1 if any of the search items in SearchString can be found
    'in FullText$. Returns 0 if no search terms are found.
    'Multiple items in SearchString$ must be in the format "term1+term2+..."

    IF LEN(FullText$) = 0 THEN EXIT FUNCTION
    IF LEN(SearchString$) = 0 THEN EXIT FUNCTION

    FOR i = 1 TO LEN(SearchString$)
        IF (ASC(SearchString$, i)) = 43 THEN
            IF LEN(ThisTerm$) > 0 THEN
                IF INSTR(FullText$, TRIM$(ThisTerm$)) > 0 THEN MULTI_SEARCH = -1: EXIT FUNCTION
                ThisTerm$ = ""
            END IF
        ELSE
            ThisTerm$ = ThisTerm$ + MID$(SearchString$, i, 1)
        END IF
    NEXT i
    IF LEN(ThisTerm$) > 0 THEN
        IF INSTR(FullText$, TRIM$(ThisTerm$)) > 0 THEN MULTI_SEARCH = -1
    END IF
END SUB

'------------------------------------------------------------------------------
SUB RESTORE_LIBRARY
    'Restores "timers.h" in the same folder as vWATCH64 is (hopefully
    'it's also the folder QB64 is). The file "timers.h" needs to be
    'in QB64's folder as it's needed to compile the output code.
    '
    'C code provided courtesy of Luke Ceddia.

    IF _FILEEXISTS("timers.h") THEN EXIT SUB

    SourceLine$ = SourceLine$ + "extern int32 ontimerthread_lock;" + CHR$(LF)
    SourceLine$ = SourceLine$ + "void stop_timers() {" + CHR$(LF)
    SourceLine$ = SourceLine$ + "  ontimerthread_lock = 1;" + CHR$(LF)
    SourceLine$ = SourceLine$ + "  while (ontimerthread_lock != 2);" + CHR$(LF)
    SourceLine$ = SourceLine$ + "}" + CHR$(LF) + CHR$(LF)
    SourceLine$ = SourceLine$ + "void start_timers() {" + CHR$(LF)
    SourceLine$ = SourceLine$ + "  ontimerthread_lock = 0;" + CHR$(LF)
    SourceLine$ = SourceLine$ + "}" + CHR$(LF)

    LibOutput = FREEFILE
    FILEERRORRAISED = 0
    ON ERROR GOTO FileError
    OPEN "timers.h" FOR BINARY AS #LibOutput
    IF FILEERRORRAISED THEN SYSTEM_BEEP 0: PRINT "FATAL ERROR: Cannot write 'timers.h' to "; _CWD$: SLEEP: SYSTEM
    PUT #LibOutput, 1, SourceLine$
    CLOSE #LibOutput
END SUB

'------------------------------------------------------------------------------
FUNCTION ADLER32$ (DataArray$, ShowProgress AS _BYTE)
    'This function comes from Videogamer555. Read the original topic below:
    'http://www.qb64.net/forum/index.php?topic=2804.msg24245#msg24245
    DIM A32$
    DIM Astr AS STRING * 4
    DIM Bstr AS STRING * 4
    A = 1
    B = 0

    UpdateStep = LEN(DataArray$) / 10
    FOR i = 1 TO LEN(DataArray$)
        IF ShowProgress AND i > UpdateStep THEN
            UpdateStep = UpdateStep + (LEN(DataArray$) / 10)
            StatusMessage$ = TRIM$(STR$(i)) + "/" + TRIM$(STR$(LEN(DataArray$)))
            Message$ = ""
            Message$ = Message$ + "Calculating checksum..." + CHR$(LF)
            Message$ = Message$ + StatusMessage$
            PROGRESSBOX "Processing...", Message$, LEN(DataArray$), i
            IF _KEYHIT = -27 THEN EXIT SUB
        END IF
        A = (A + ASC(MID$(DataArray$, i, 1))) MOD 65521
        B = (B + A) MOD 65521
    NEXT i
    RSET Astr = HEX$(A)
    RSET Bstr = HEX$(B)

    A32$ = Bstr + Astr
    FOR i = 1 TO 8
        IF MID$(A32$, i, 1) = " " THEN MID$(A32$, i, 1) = "0"
    NEXT i

    ADLER32$ = A32$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION GETLINE$ (TargetLine AS LONG)
    IF LEN(SOURCEFILE) > 0 AND TargetLine > 0 AND TargetLine <= UBOUND(SOURCECODE) THEN
        GETLINE$ = SOURCECODE(TargetLine)
    END IF
END FUNCTION

'------------------------------------------------------------------------------
SUB SEND_PING
    'Check if the connection is still alive on the client's end
    $IF WIN THEN
        hnd& = OpenProcess(&H400, 0, CLIENT.PID)
        b& = GetExitCodeProcess(hnd&, ExitCode&)
        IF b& = 1 AND ExitCode& = 259 THEN
            'Debuggee is active.
        ELSE
            'Debuggee was closed.
            DEBUGGEE_CLOSED = -1
        END IF
        b& = CloseHandle(hnd&)
    $ELSE
        IF PROCESS_CLOSED(CLIENT.PID, 0) THEN DEBUGGEE_CLOSED = -1
    $END IF
END SUB

'------------------------------------------------------------------------------
FUNCTION FIND_KEYWORD (Text$, SearchTerm$, SearchTermFound)
    SEP$ = " =<>+-/\^:;,*()!#%&`$"
    T$ = UCASE$(TRIM$(STRIPCOMMENTS$(Text$)))
    S$ = UCASE$(TRIM$(SearchTerm$))
    T.L = LEN(Text$) - LEN(LTRIM$(Text$))
    S.L = LEN(S$)
    SearchTermFound = 0

    IF LEFT$(T$, S.L) = S$ AND INSTR(SEP$, MID$(T$, S.L + 1, 1)) > 0 THEN
        SearchTermFound = T.L + 1
        FIND_KEYWORD = -1
        EXIT FUNCTION
    END IF

    DO
        SearchTermFound = INSTR(SearchTermFound + 1, T$, S$)
        IF SearchTermFound = 0 THEN EXIT FUNCTION
        IF SearchTermFound > 1 THEN CharBefore$ = MID$(T$, SearchTermFound - 1, 1) ELSE CharBefore$ = " "
        IF SearchTermFound + S.L <= LEN(T$) THEN CharAfter$ = MID$(T$, SearchTermFound + S.L, 1) ELSE CharAfter$ = " "
    LOOP UNTIL (INSTR(SEP$, CharBefore$) > 0 AND INSTR(SEP$, CharAfter$) > 0)

    'Checks if SearchTerm$ is outside quotation marks
    FOR i = 1 TO SearchTermFound - 1
        IF ASC(T$, i) = 34 THEN
            OpenQuotation = NOT OpenQuotation
        END IF
    NEXT i
    SearchTermFound = SearchTermFound + T.L
    IF NOT OpenQuotation THEN FIND_KEYWORD = -1
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION INTERVAL_SEARCH (Filter$, i)
    'Filter must contain a valid numeric string (####),
    'a valid interval (####-####) or comma-separated values.

    IF LEN(Filter$) = 0 THEN EXIT FUNCTION
    IF i = 0 THEN EXIT FUNCTION

    Separator = INSTR(Filter$, "-") + INSTR(Filter$, ",")

    IF Separator = 0 THEN 'Single number passed
        IF VAL(Filter$) = i THEN INTERVAL_SEARCH = -1
        EXIT FUNCTION
    END IF

    Reading = 1
    FOR j = 1 TO LEN(Filter$)
        v = ASC(Filter$, j)
        SELECT CASE v
            CASE 44 'comma
                Reading = 1
                GOSUB Eval
            CASE 45 'hyphen
                IF PrevChar <> 45 THEN
                    Reading = Reading + 1
                    IF Reading = 2 THEN
                        IF j = LEN(Filter$) THEN GOSUB Eval
                    END IF
                END IF
            CASE 48 TO 57 '0 to 9
                IF Reading = 1 THEN
                    v1$ = v1$ + CHR$(v)
                ELSEIF Reading = 2 THEN
                    v2$ = v2$ + CHR$(v)
                END IF
                IF j = LEN(Filter$) THEN GOSUB Eval
        END SELECT
        PrevChar = v
    NEXT j

    EXIT FUNCTION
    Eval:
    v1 = VAL(v1$)
    v2 = VAL(v2$)
    v1$ = ""
    v2$ = ""
    IF v2 > 0 THEN
        IF v1 > 0 THEN
            IF v1 > v2 THEN v2 = v1
            IF i >= v1 AND i <= v2 THEN INTERVAL_SEARCH = -1
        END IF
    ELSE
        IF v1 = i THEN INTERVAL_SEARCH = -1
    END IF
    RETURN
END FUNCTION

'------------------------------------------------------------------------------
SUB DISPLAYSCROLLBAR (y, grabbedY, SB_ThumbY, SB_ThumbH, SB_Ratio AS SINGLE, mx, my)
    IF PAGE_HEIGHT <= LIST_AREA THEN EXIT SUB

    SB_Ratio = LIST_AREA / PAGE_HEIGHT
    SB_TRACK = LIST_AREA - 48
    SB_ThumbH = SB_TRACK * SB_Ratio
    IF SB_ThumbH < 20 THEN SB_ThumbH = 20

    SB_ThumbY = (SB_TRACK - SB_ThumbH) * (y / (PAGE_HEIGHT - LIST_AREA))

    'Draw scrollbar
    LINE (_WIDTH - 30, SCREEN_TOPBAR + 1)-STEP(29, LIST_AREA), _RGB32(170, 170, 170), BF

    'Draw buttons
    IF mx > _WIDTH - 30 AND my > SCREEN_TOPBAR + 1 THEN
        'Highlight arrows if hovererd
        IF my <= SCREEN_TOPBAR + 21 THEN
            LINE (_WIDTH - 30, SCREEN_TOPBAR + 1)-STEP(29, 20), _RGBA32(230, 230, 230, 235), BF
        ELSEIF my >= SCREEN_HEIGHT - ((TOTAL_SELECTEDVARIABLES + 1) * _FONTHEIGHT) - 21 THEN
            LINE (_WIDTH - 30, SCREEN_HEIGHT - ((TOTAL_SELECTEDVARIABLES + 1) * _FONTHEIGHT) - 21)-STEP(29, 20), _RGBA32(230, 230, 230, 235), BF
        END IF
    END IF
    _PRINTSTRING (_WIDTH - 20, SCREEN_TOPBAR + 5), CHR$(24)
    _PRINTSTRING (_WIDTH - 20, SCREEN_HEIGHT - ((TOTAL_SELECTEDVARIABLES + 1) * _FONTHEIGHT) - _FONTHEIGHT - 5), CHR$(25)

    IF grabbedY = -1 THEN
        SB_StartX = 25
        SB_ThumbW = 19
        SB_ThumbColor = _RGB32(70, 70, 70)
    ELSE
        SB_StartX = 24
        SB_ThumbW = 17
        SB_ThumbColor = _RGB32(0, 0, 0)
        SB_ThumbY = SB_ThumbY + 1
        SB_ThumbH = SB_ThumbH - 2
    END IF

    'Draw thumb
    LINE (_WIDTH - SB_StartX, SB_ThumbY + SCREEN_TOPBAR + 24)-STEP(SB_ThumbW, SB_ThumbH), SB_ThumbColor, BF
END SUB

'------------------------------------------------------------------------------
SUB CHECK_SCREEN_LIMITS (y)
    IF y < 0 THEN y = 0
    IF PAGE_HEIGHT > LIST_AREA THEN
        IF y > PAGE_HEIGHT - LIST_AREA THEN y = PAGE_HEIGHT - LIST_AREA
    ELSE
        y = 0
    END IF
END SUB

'------------------------------------------------------------------------------
SUB CHECK_RESIZE (new_w%, new_h%)
    DIM fg AS LONG, bg AS LONG

    IF _RESIZE = 0 THEN
        IF new_w% + new_h% = 0 THEN EXIT SUB
    ELSE
        DO: LOOP WHILE _RESIZE
        new_w% = _RESIZEWIDTH
        new_h% = _RESIZEHEIGHT
    END IF

    IF new_w% = SCREEN_WIDTH AND new_h% = SCREEN_HEIGHT THEN EXIT SUB

    IF new_w% < DEFAULT_WIDTH THEN new_w% = DEFAULT_WIDTH
    IF new_h% < SCREEN_TOPBAR * 3 THEN new_h% = SCREEN_TOPBAR * 3

    SCREEN_WIDTH = new_w%
    SCREEN_HEIGHT = new_h%

    fg = _DEFAULTCOLOR: bg = _BACKGROUNDCOLOR

    SCREEN _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
    COLOR fg, bg
    _FREEIMAGE MAINSCREEN
    MAINSCREEN = _DEST

    LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR - ((TOTAL_SELECTEDVARIABLES + 1) * _FONTHEIGHT)
    SB_TRACK = LIST_AREA - 48
END SUB

'------------------------------------------------------------------------------
SUB SET_DEF (Range$, DataType$)
    FOR i = 1 TO LEN(Range$)
        SELECT CASE ASC(Range$, i)
            CASE 65 TO 90 'A to Z
                IF firstLetter = 0 THEN
                    firstLetter = ASC(Range$, i)
                    IF i = LEN(Range$) THEN
                        DEFAULTDATATYPE(firstLetter) = DataType$
                    END IF
                ELSEIF secondLetter = 0 THEN
                    secondLetter = ASC(Range$, i)
                    IF firstLetter > secondLetter THEN SWAP firstLetter, secondLetter
                    FOR j = firstLetter TO secondLetter
                        DEFAULTDATATYPE(j) = DataType$
                    NEXT j
                    firstLetter = 0
                    secondLetter = 0
                END IF
            CASE 44 'comma
                IF secondLetter = 0 THEN
                    IF firstLetter > 0 THEN DEFAULTDATATYPE(firstLetter) = DataType$
                    firstLetter = 0
                ELSE
                    firstLetter = 0
                    secondLetter = 0
                END IF
        END SELECT
    NEXT i
END SUB

'------------------------------------------------------------------------------
SUB SYSTEM_BEEP (MessageType AS INTEGER)
    $IF WIN THEN
        DIM SoundID AS STRING
        SELECT CASE MessageType
            CASE YN_QUESTION: SoundID = "SystemExclamation"
            CASE ELSE: SoundID = "SystemDefault"
        END SELECT
        x = PlaySound(SoundID + CHR$(0), 0, 65536 + 1)
    $ELSE
        BEEP
    $END IF
END SUB

'------------------------------------------------------------------------------
FUNCTION MESSAGEBOX (tTitle$, tMessage$, MessageSetup AS STRING, DefaultButton AS _BYTE, SendPing AS _BYTE)
    DIM MessageType AS INTEGER
    DIM Position AS INTEGER
    DIM TempCaption$

    MessageType = CVI(LEFT$(MessageSetup, 2))
    Message$ = tMessage$
    Title$ = TRIM$(tTitle$)
    IF Title$ = "" THEN Title$ = ID

    CharW = _PRINTWIDTH("_")
    REDIM MessageLines(1) AS STRING
    PCOPY 1, 0
    LINE (0, 0)-STEP(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1), _RGBA32(170, 170, 170, 170), BF
    MaxLen = 1
    DO
        lineBreak = INSTR(lineBreak + 1, Message$, CHR$(LF))
        IF lineBreak = 0 AND totalLines = 0 THEN
            totalLines = 1
            MessageLines(1) = Message$
            MaxLen = LEN(Message$)
            EXIT DO
        ELSEIF lineBreak = 0 AND totalLines > 0 THEN
            totalLines = totalLines + 1
            REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
            MessageLines(totalLines) = RIGHT$(Message$, LEN(Message$) - prevlinebreak + 1)
            IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
            EXIT DO
        END IF
        IF totalLines = 0 THEN prevlinebreak = 1
        totalLines = totalLines + 1
        REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
        MessageLines(totalLines) = MID$(Message$, prevlinebreak, lineBreak - prevlinebreak)
        IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
        prevlinebreak = lineBreak + 1
    LOOP

    DialogW = (CharW * MaxLen) + 20
    DialogH = _FONTHEIGHT * (4 + totalLines) + 10
    DialogX = _WIDTH(MAINSCREEN) / 2 - DialogW / 2
    DialogY = _HEIGHT(MAINSCREEN) / 2 - DialogH / 2

    SELECT CASE MessageType
        CASE MB_CUSTOM
            Position = 3 'skip MKI$ data
            TotalButtons = 0
            ButtonLine$ = " "
            REDIM Buttons(TotalButtons) AS BUTTONSTYPE
            FOR cb = Position TO LEN(MessageSetup)
                IF ASC(MessageSetup, cb) = LF THEN
                    GOSUB AddButton
                    TempCaption$ = ""
                ELSE
                    TempCaption$ = TempCaption$ + MID$(MessageSetup, cb, 1)
                END IF
            NEXT cb
            IF LEN(TempCaption$) > 0 THEN GOSUB AddButton
            Buttons(1).X = (_WIDTH / 2) - (_PRINTWIDTH(ButtonLine$) / 2) + _PRINTWIDTH(SPACE$(INSTR(ButtonLine$, TRIM$(Buttons(1).CAPTION))))
            FOR cb = 2 TO TotalButtons
                Buttons(cb).X = (_WIDTH / 2) - (_PRINTWIDTH(ButtonLine$) / 2) + _PRINTWIDTH(SPACE$(INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION))))
            NEXT cb
            IF LEN(ButtonLine$) > MaxLen THEN
                MaxLen = LEN(ButtonLine$)
                DialogW = (CharW * MaxLen) + 20
                DialogX = _WIDTH(MAINSCREEN) / 2 - DialogW / 2
            END IF
        CASE OK_ONLY
            TotalButtons = 1
            DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE
            b = 1
            Buttons(b).ID = 1
            Buttons(b).CAPTION = "< OK >"
            Buttons(b).Y = DialogY + 5 + _FONTHEIGHT * (3 + totalLines)
            Buttons(b).X = _WIDTH / 2 - _PRINTWIDTH(TRIM$(Buttons(b).CAPTION)) / 2
            Buttons(b).W = _PRINTWIDTH(TRIM$(Buttons(b).CAPTION))
        CASE YN_QUESTION
            TotalButtons = 2
            DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE
            b = 1
            Buttons(b).ID = 1: Buttons(b).CAPTION = "< Yes >": b = b + 1
            Buttons(b).ID = 2: Buttons(b).CAPTION = "< No  >": b = b + 1
            ButtonLine$ = " "
            FOR cb = 1 TO TotalButtons
                ButtonLine$ = ButtonLine$ + TRIM$(Buttons(cb).CAPTION) + " "
                Buttons(cb).Y = DialogY + 5 + _FONTHEIGHT * (3 + totalLines)
                Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
            NEXT cb
            Buttons(1).X = (_WIDTH / 2) - (_PRINTWIDTH(ButtonLine$) / 2) + _PRINTWIDTH(SPACE$(INSTR(ButtonLine$, TRIM$(Buttons(1).CAPTION))))
            FOR cb = 2 TO TotalButtons
                Buttons(cb).X = (_WIDTH / 2) - (_PRINTWIDTH(ButtonLine$) / 2) + _PRINTWIDTH(SPACE$(INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION))))
            NEXT cb
    END SELECT

    SYSTEM_BEEP MessageType
    DIALOGRESULT = 0
    _KEYCLEAR
    DO
        LINE (DialogX, DialogY)-STEP(DialogW, DialogH), _RGB32(255, 255, 255), BF
        LINE (DialogX, DialogY)-STEP(DialogW, _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(Title$) / 2, DialogY + 1), Title$

        COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
        FOR i = 1 TO totalLines
            Message.X = _WIDTH / 2 - _PRINTWIDTH(MessageLines(i)) / 2
            _PRINTSTRING (Message.X, DialogY + 5 + _FONTHEIGHT * (i + 1)), MessageLines(i)
        NEXT i

        'Draw buttons
        GOSUB CheckButtons
        FOR cb = 1 TO TotalButtons
            _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), TRIM$(Buttons(cb).CAPTION)
            IF cb = DefaultButton THEN
                COLOR _RGB32(255, 255, 0)
                _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), "<" + SPACE$(LEN(TRIM$(Buttons(cb).CAPTION)) - 2) + ">"
                COLOR _RGB32(0, 178, 179)
                _PRINTSTRING (Buttons(cb).X - 1, Buttons(cb).Y - 1), "<" + SPACE$(LEN(TRIM$(Buttons(cb).CAPTION)) - 2) + ">"
                COLOR _RGB32(0, 0, 0)
            END IF
        NEXT cb
        'end of drawing buttons

        _DISPLAY
        modKey = _KEYHIT: k = modKey
        IF modKey = 100303 OR modKey = 100304 THEN shiftDown = -1
        IF modKey = -100303 OR modKey = -100304 THEN shiftDown = 0
        IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
        IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0

        SELECT CASE MessageType
            CASE MB_CUSTOM
                IF k = 13 THEN DIALOGRESULT = DefaultButton
                IF k = 27 THEN DIALOGRESULT = -1
                IF k = 19712 THEN GOTO NextItem
                IF k = 19200 THEN GOTO PrevItem
                IF k = 25 THEN GOTO PrevItem
                IF k = 9 AND shiftDown = 0 THEN
                    NextItem:
                    DefaultButton = DefaultButton + 1: IF DefaultButton > TotalButtons THEN DefaultButton = 1
                END IF
                IF k = 9 AND shiftDown = -1 THEN
                    PrevItem:
                    DefaultButton = DefaultButton - 1: IF DefaultButton < 1 THEN DefaultButton = TotalButtons
                END IF
            CASE OK_ONLY
                IF k = 13 OR k = 32 THEN DIALOGRESULT = 1
                IF k = 27 THEN DIALOGRESULT = 2
            CASE YN_QUESTION
                IF k = 13 OR k = 32 THEN DIALOGRESULT = DefaultButton + 5
                IF k = 27 THEN DIALOGRESULT = MB_NO
                IF k = 19712 THEN GOTO NextItem
                IF k = 19200 THEN GOTO PrevItem
                IF k = 25 THEN GOTO PrevItem
                IF k = 9 AND shiftDown = 0 THEN
                    DefaultButton = DefaultButton + 1: IF DefaultButton > TotalButtons THEN DefaultButton = 1
                END IF
                IF k = 9 AND shiftDown = -1 THEN
                    DefaultButton = DefaultButton - 1: IF DefaultButton < 1 THEN DefaultButton = TotalButtons
                END IF
                IF k = 89 OR k = 121 THEN DIALOGRESULT = MB_YES
                IF k = 78 OR k = 110 THEN DIALOGRESULT = MB_NO
        END SELECT
        IF _EXIT THEN USERQUIT = -1: EXIT DO
        IF SendPing THEN SEND_PING
    LOOP UNTIL DIALOGRESULT > 0 OR DIALOGRESULT = -1
    _KEYCLEAR
    MESSAGEBOX = DIALOGRESULT
    PCOPY 1, 0
    EXIT SUB

    CheckButtons:
    'Grab mouse data:
    WHILE _MOUSEINPUT: WEND
    mb = _MOUSEBUTTON(1): mx = _MOUSEX: my = _MOUSEY

    'Hover highlight:
    FOR cb = 1 TO TotalButtons
        IF ((mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W)) OR cb = DefaultButton THEN
            IF ((my >= Buttons(cb).Y) AND (my < Buttons(cb).Y + _FONTHEIGHT)) OR cb = DefaultButton THEN
                LINE (Buttons(cb).X, Buttons(cb).Y)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
            END IF
        END IF
    NEXT cb

    IF mb THEN
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                IF (my >= Buttons(cb).Y) AND (my < Buttons(cb).Y + _FONTHEIGHT) THEN
                    DefaultButton = cb
                    WHILE _MOUSEBUTTON(1): _LIMIT 500: mb = _MOUSEINPUT: WEND
                    mb = 0: nmx = _MOUSEX: nmy = _MOUSEY
                    IF nmx = mx AND nmy = my THEN
                        SELECT CASE MessageType
                            CASE OK_ONLY, MB_CUSTOM: DIALOGRESULT = cb
                            CASE YN_QUESTION: DIALOGRESULT = cb + 5
                        END SELECT
                    END IF
                    RETURN
                END IF
            END IF
        NEXT cb
    END IF
    RETURN

    AddButton:
    TotalButtons = TotalButtons + 1
    REDIM _PRESERVE Buttons(1 TO TotalButtons) AS BUTTONSTYPE
    Buttons(TotalButtons).CAPTION = "< " + TempCaption$ + " >"
    ButtonLine$ = ButtonLine$ + TRIM$(Buttons(TotalButtons).CAPTION) + " "
    Buttons(TotalButtons).Y = DialogY + 5 + _FONTHEIGHT * (3 + totalLines)
    Buttons(TotalButtons).W = _PRINTWIDTH(TRIM$(Buttons(TotalButtons).CAPTION))
    Buttons(TotalButtons).ID = TotalButtons
    RETURN
END FUNCTION

'------------------------------------------------------------------------------
SUB PROGRESSBOX (tTitle$, tMessage$, MaxValue, Value)
    Message$ = tMessage$
    Title$ = TRIM$(tTitle$)
    IF Title$ = "" THEN Title$ = ID

    CharW = _PRINTWIDTH("_")
    REDIM MessageLines(1) AS STRING
    PCOPY 1, 0
    LINE (0, 0)-STEP(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1), _RGBA32(170, 170, 170, 170), BF
    MaxLen = 1
    DO
        lineBreak = INSTR(lineBreak + 1, Message$, CHR$(LF))
        IF lineBreak = 0 AND totalLines = 0 THEN
            totalLines = 1
            MessageLines(1) = Message$
            MaxLen = LEN(Message$)
            EXIT DO
        ELSEIF lineBreak = 0 AND totalLines > 0 THEN
            totalLines = totalLines + 1
            REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
            MessageLines(totalLines) = RIGHT$(Message$, LEN(Message$) - prevlinebreak + 1)
            IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
            EXIT DO
        END IF
        IF totalLines = 0 THEN prevlinebreak = 1
        totalLines = totalLines + 1
        REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
        MessageLines(totalLines) = MID$(Message$, prevlinebreak, lineBreak - prevlinebreak)
        IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
        prevlinebreak = lineBreak + 1
    LOOP

    ProgressBarLength = 62
    ThisStepLength = 62 * (Value / MaxValue)

    DialogH = _FONTHEIGHT * (5 + totalLines) + 10
    DialogW = (CharW * ProgressBarLength) + 10
    IF DialogW < MaxLen * CharW + 10 THEN DialogW = MaxLen * CharW + 10

    DialogX = _WIDTH(MAINSCREEN) / 2 - DialogW / 2
    DialogY = _HEIGHT(MAINSCREEN) / 2 - DialogH / 2
    ProgressBar.X = (DialogX + (DialogW / 2)) - (((ProgressBarLength * CharW) - 10) / 2) - 4

    LINE (DialogX, DialogY)-STEP(DialogW - 1, DialogH - 1), _RGB32(255, 255, 255), BF
    LINE (DialogX, DialogY)-STEP(DialogW - 1, _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(Title$) / 2, DialogY + 1), Title$

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    FOR i = 1 TO totalLines
        Message.X = _WIDTH / 2 - _PRINTWIDTH(MessageLines(i)) / 2
        _PRINTSTRING (Message.X, DialogY + 5 + _FONTHEIGHT * (i + 1)), MessageLines(i)
    NEXT i

    'Progress bar
    LINE (ProgressBar.X - 2, DialogY + 3 + _FONTHEIGHT * (3 + totalLines))-STEP(ProgressBarLength * CharW, _FONTHEIGHT + 4), _RGB32(200, 200, 200), BF
    LINE (ProgressBar.X - 2, DialogY + 3 + _FONTHEIGHT * (3 + totalLines))-STEP(ThisStepLength * CharW, _FONTHEIGHT + 4), _RGB32(0, 178, 179), BF

    _DISPLAY
END SUB

'------------------------------------------------------------------------------
FUNCTION INPUTBOX (tTitle$, tMessage$, InitialValue AS STRING, NewValue AS STRING, Selected, SendPing AS _BYTE)
    'Show a dialog and allow user input. Returns 1 = OK or 2 = Cancel.
    'ReturnValue is always a string: caller procedure must convert it.
    Message$ = tMessage$
    Title$ = TRIM$(tTitle$)
    IF Title$ = "" THEN Title$ = ID
    NewValue = TRIM$(InitialValue)

    CharW = _PRINTWIDTH("_")
    REDIM MessageLines(1) AS STRING
    PCOPY 1, 0
    LINE (0, 0)-STEP(SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1), _RGBA32(170, 170, 170, 170), BF
    MaxLen = 1
    DO
        lineBreak = INSTR(lineBreak + 1, Message$, CHR$(LF))
        IF lineBreak = 0 AND totalLines = 0 THEN
            totalLines = 1
            MessageLines(1) = Message$
            MaxLen = LEN(Message$)
            EXIT DO
        ELSEIF lineBreak = 0 AND totalLines > 0 THEN
            totalLines = totalLines + 1
            REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
            MessageLines(totalLines) = RIGHT$(Message$, LEN(Message$) - prevlinebreak + 1)
            IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
            EXIT DO
        END IF
        IF totalLines = 0 THEN prevlinebreak = 1
        totalLines = totalLines + 1
        REDIM _PRESERVE MessageLines(1 TO totalLines) AS STRING
        MessageLines(totalLines) = MID$(Message$, prevlinebreak, lineBreak - prevlinebreak)
        IF LEN(MessageLines(totalLines)) > MaxLen THEN MaxLen = LEN(MessageLines(totalLines))
        prevlinebreak = lineBreak + 1
    LOOP

    Cursor = LEN(NewValue)
    Selection.Start = 0
    InputViewStart = 1
    FieldArea = 62
    IF Selected > 0 THEN Selection.Start = Selected: Selected = -1

    DialogH = _FONTHEIGHT * (6 + totalLines) + 10
    DialogW = (CharW * FieldArea) + 10
    IF DialogW < MaxLen * CharW + 10 THEN DialogW = MaxLen * CharW + 10

    DialogX = _WIDTH(MAINSCREEN) / 2 - DialogW / 2
    DialogY = _HEIGHT(MAINSCREEN) / 2 - DialogH / 2
    InputField.X = (DialogX + (DialogW / 2)) - (((FieldArea * CharW) - 10) / 2) - 4

    TotalButtons = 2
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "< OK >": b = b + 1
    Buttons(b).ID = 2: Buttons(b).CAPTION = "< Cancel >": b = b + 1
    ButtonLine$ = " "
    FOR cb = 1 TO TotalButtons
        ButtonLine$ = ButtonLine$ + TRIM$(Buttons(cb).CAPTION) + " "
        Buttons(cb).Y = DialogY + 5 + _FONTHEIGHT * (5 + totalLines)
        Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
    NEXT cb
    Buttons(1).X = _WIDTH / 2 - _PRINTWIDTH(ButtonLine$) / 2
    FOR cb = 2 TO TotalButtons
        Buttons(cb).X = Buttons(1).X + _PRINTWIDTH(SPACE$(INSTR(ButtonLine$, TRIM$(Buttons(cb).CAPTION))))
    NEXT cb

    DIALOGRESULT = 0
    _KEYCLEAR
    DO: _LIMIT 500
        LINE (DialogX, DialogY)-STEP(DialogW - 1, DialogH - 1), _RGB32(255, 255, 255), BF
        LINE (DialogX, DialogY)-STEP(DialogW - 1, _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(Title$) / 2, DialogY + 1), Title$

        COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
        FOR i = 1 TO totalLines
            Message.X = _WIDTH / 2 - _PRINTWIDTH(MessageLines(i)) / 2
            _PRINTSTRING (Message.X, DialogY + 5 + _FONTHEIGHT * (i + 1)), MessageLines(i)
        NEXT i

        'Input field
        LINE (InputField.X - 2, DialogY + 3 + _FONTHEIGHT * (3 + totalLines))-STEP(FieldArea * CharW, _FONTHEIGHT + 4), _RGB32(200, 200, 200), BF
        _PRINTSTRING (InputField.X, DialogY + 5 + _FONTHEIGHT * (3 + totalLines)), MID$(NewValue, InputViewStart, FieldArea)

        'Selection highlight:
        GOSUB SelectionHighlight

        'Cursor:
        IF TIMER - SetCursor# > .4 THEN
            SetCursor# = TIMER
            IF cursorBlink% = 1 THEN cursorBlink% = 0 ELSE cursorBlink% = 1
        END IF
        IF cursorBlink% = 1 THEN
            LINE (InputField.X + (Cursor - (InputViewStart - 1)) * CharW, DialogY + 5 + _FONTHEIGHT * (3 + totalLines))-STEP(0, _FONTHEIGHT), _RGB32(0, 0, 0)
        END IF

        'Draw buttons
        GOSUB CheckButtons
        FOR cb = 1 TO TotalButtons
            _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), TRIM$(Buttons(cb).CAPTION)
            IF cb = DefaultButton THEN
                COLOR _RGB32(255, 255, 0)
                _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), "<" + SPACE$(LEN(TRIM$(Buttons(cb).CAPTION)) - 2) + ">"
                COLOR _RGB32(0, 178, 179)
                _PRINTSTRING (Buttons(cb).X - 1, Buttons(cb).Y - 1), "<" + SPACE$(LEN(TRIM$(Buttons(cb).CAPTION)) - 2) + ">"
                COLOR _RGB32(0, 0, 0)
            END IF
        NEXT cb
        'end of drawing buttons

        _DISPLAY
        modKey = _KEYHIT: k = modKey
        IF modKey = 100303 OR modKey = 100304 THEN shiftDown = -1
        IF modKey = -100303 OR modKey = -100304 THEN shiftDown = 0
        IF modKey = 100305 OR modKey = 100306 THEN ctrlDown = -1
        IF modKey = -100305 OR modKey = -100306 THEN ctrlDown = 0

        SELECT CASE k
            CASE 13: DIALOGRESULT = 1
            CASE 27: DIALOGRESULT = 2
            CASE 32 TO 126 'Printable ASCII characters
                IF k = ASC("v") OR k = ASC("V") THEN
                    IF ctrlDown THEN
                        Clip$ = TRUNCATE$(_CLIPBOARD$, 13)
                        Clip$ = TRUNCATE$(Clip$, 10)
                        IF LEN(TRIM$(Clip$)) > 0 THEN
                            IF NOT Selected THEN
                                IF Cursor = LEN(NewValue) THEN
                                    NewValue = NewValue + Clip$
                                    Cursor = LEN(NewValue)
                                ELSE
                                    NewValue = LEFT$(NewValue, Cursor) + Clip$ + MID$(NewValue, Cursor + 1)
                                    Cursor = Cursor + LEN(Clip$)
                                END IF
                            ELSE
                                s1 = Selection.Start
                                s2 = Cursor
                                IF s1 > s2 THEN SWAP s1, s2
                                NewValue = LEFT$(NewValue, s1) + Clip$ + MID$(NewValue, s2 + 1)
                                Cursor = s1 + LEN(Clip$)
                                Selected = 0
                            END IF
                        END IF
                        k = 0
                    END IF
                ELSEIF k = ASC("c") OR k = ASC("C") THEN
                    IF ctrlDown THEN
                        _CLIPBOARD$ = Selection.Value$
                        k = 0
                    END IF
                ELSEIF k = ASC("x") OR k = ASC("X") THEN
                    IF ctrlDown THEN
                        _CLIPBOARD$ = Selection.Value$
                        GOSUB DeleteSelection
                        k = 0
                    END IF
                ELSEIF k = ASC("a") OR k = ASC("A") THEN
                    IF ctrlDown THEN
                        Cursor = LEN(NewValue)
                        Selection.Start = 0
                        Selected = -1
                        k = 0
                    END IF
                END IF

                IF k > 0 THEN
                    IF NOT Selected THEN
                        IF Cursor = LEN(NewValue) THEN
                            NewValue = NewValue + CHR$(k)
                            Cursor = Cursor + 1
                        ELSE
                            NewValue = LEFT$(NewValue, Cursor) + CHR$(k) + MID$(NewValue, Cursor + 1)
                            Cursor = Cursor + 1
                        END IF
                        IF Cursor > FieldArea THEN InputViewStart = (Cursor - FieldArea) + 2
                    ELSE
                        s1 = Selection.Start
                        s2 = Cursor
                        IF s1 > s2 THEN SWAP s1, s2
                        NewValue = LEFT$(NewValue, s1) + CHR$(k) + MID$(NewValue, s2 + 1)
                        Selected = 0
                        Cursor = s1 + 1
                    END IF
                END IF
            CASE 8 'Backspace
                IF LEN(NewValue) > 0 THEN
                    IF NOT Selected THEN
                        IF Cursor = LEN(NewValue) THEN
                            NewValue = LEFT$(NewValue, LEN(NewValue) - 1)
                            Cursor = Cursor - 1
                        ELSEIF Cursor > 1 THEN
                            NewValue = LEFT$(NewValue, Cursor - 1) + MID$(NewValue, Cursor + 1)
                            Cursor = Cursor - 1
                        ELSEIF Cursor = 1 THEN
                            NewValue = RIGHT$(NewValue, LEN(NewValue) - 1)
                            Cursor = Cursor - 1
                        END IF
                    ELSE
                        GOSUB DeleteSelection
                    END IF
                END IF
            CASE 21248 'Delete
                IF NOT Selected THEN
                    IF LEN(NewValue) > 0 THEN
                        IF Cursor = 0 THEN
                            NewValue = RIGHT$(NewValue, LEN(NewValue) - 1)
                        ELSEIF Cursor > 0 AND Cursor <= LEN(NewValue) - 1 THEN
                            NewValue = LEFT$(NewValue, Cursor) + MID$(NewValue, Cursor + 2)
                        END IF
                    END IF
                ELSE
                    GOSUB DeleteSelection
                END IF
            CASE 19200 'Left arrow key
                GOSUB CheckSelection
                IF Cursor > 0 THEN Cursor = Cursor - 1
            CASE 19712 'Right arrow key
                GOSUB CheckSelection
                IF Cursor < LEN(NewValue) THEN Cursor = Cursor + 1
            CASE 18176 'Home
                GOSUB CheckSelection
                Cursor = 0
            CASE 20224 'End
                GOSUB CheckSelection
                Cursor = LEN(NewValue)
        END SELECT

        'Cursor adjustments:
        GOSUB CursorAdjustments

        IF _EXIT THEN USERQUIT = -1: EXIT DO
        IF SendPing THEN SEND_PING
    LOOP UNTIL DIALOGRESULT > 0

    _KEYCLEAR
    INPUTBOX = DIALOGRESULT
    PCOPY 1, 0

    EXIT SUB

    CursorAdjustments:
    IF Cursor > prevCursor THEN
        IF Cursor - InputViewStart + 2 > FieldArea THEN InputViewStart = (Cursor - FieldArea) + 2
    ELSEIF Cursor < prevCursor THEN
        IF Cursor < InputViewStart - 1 THEN InputViewStart = Cursor
    END IF
    prevCursor = Cursor
    IF InputViewStart < 1 THEN InputViewStart = 1
    RETURN

    CheckSelection:
    IF shiftDown = -1 THEN
        IF Selected = 0 THEN
            Selected = -1
            Selection.Start = Cursor
        END IF
    ELSEIF shiftDown = 0 THEN
        Selected = 0
    END IF
    RETURN

    DeleteSelection:
    NewValue = LEFT$(NewValue, s1) + MID$(NewValue, s2 + 1)
    Selected = 0
    Cursor = s1
    RETURN

    SelectionHighlight:
    IF Selected THEN
        s1 = Selection.Start
        s2 = Cursor
        IF s1 > s2 THEN
            SWAP s1, s2
            IF InputViewStart > 1 THEN
                ss1 = s1 - InputViewStart + 1
            ELSE
                ss1 = s1
            END IF
            ss2 = s2 - s1
            IF ss1 + ss2 > FieldArea THEN ss2 = FieldArea - ss1
        ELSE
            ss1 = s1
            ss2 = s2 - s1
            IF ss1 < InputViewStart THEN ss1 = 0: ss2 = s2 - InputViewStart + 1
            IF ss1 > InputViewStart THEN ss1 = ss1 - InputViewStart + 1: ss2 = s2 - s1
        END IF
        Selection.Value$ = MID$(NewValue, s1 + 1, s2 - s1)

        LINE (InputField.X + ss1 * CharW, DialogY + 5 + _FONTHEIGHT * (3 + totalLines))-STEP(ss2 * CharW, _FONTHEIGHT), _RGBA32(255, 255, 255, 150), BF
    END IF
    RETURN

    CheckButtons:
    'Hover highlight:
    WHILE _MOUSEINPUT: WEND
    mb = _MOUSEBUTTON(1): mx = _MOUSEX: my = _MOUSEY
    FOR cb = 1 TO TotalButtons
        IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
            IF (my >= Buttons(cb).Y) AND (my < Buttons(cb).Y + _FONTHEIGHT) THEN
                LINE (Buttons(cb).X, Buttons(cb).Y)-STEP(Buttons(cb).W, _FONTHEIGHT - 1), _RGBA32(230, 230, 230, 235), BF
            END IF
        END IF
    NEXT cb

    IF mb THEN
        IF mx >= InputField.X AND my >= DialogY + 3 + _FONTHEIGHT * (3 + totalLines) AND mx <= InputField.X + (FieldArea * CharW - 10) AND my <= DialogY + 3 + _FONTHEIGHT * (3 + totalLines) + _FONTHEIGHT + 4 THEN
            'Click inside the text field
            WHILE _MOUSEBUTTON(1)
                _LIMIT 500
                IF SendPing THEN SEND_PING
                mb = _MOUSEINPUT
            WEND
            Cursor = ((mx - InputField.X) / CharW) + (InputViewStart - 1)
            IF Cursor > LEN(NewValue) THEN Cursor = LEN(NewValue)
            Selected = 0
            RETURN
        END IF

        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                IF (my >= Buttons(cb).Y) AND (my < Buttons(cb).Y + _FONTHEIGHT) THEN
                    DefaultButton = cb
                    WHILE _MOUSEBUTTON(1): _LIMIT 500: mb = _MOUSEINPUT: WEND
                    mb = 0: nmx = _MOUSEX: nmy = _MOUSEY
                    IF nmx = mx AND nmy = my THEN DIALOGRESULT = cb
                    RETURN
                END IF
            END IF
        NEXT cb
    END IF
    RETURN
END FUNCTION

'------------------------------------------------------------------------------
SUB FIND_CURRENTMODULE
    'Get the name of the SUB/FUNCTION CLIENT.LINENUMBER is in.

    IF LEN(SOURCEFILE) = 0 THEN CLIENT_CURRENTMODULE = "<source unavailable>": EXIT SUB

    sfname$ = "MAIN MODULE"
    IF CLIENT.LINENUMBER > 0 THEN
        FOR currSF_CHECK = CLIENT.LINENUMBER TO 1 STEP -1
            thisline$ = GETLINE$(currSF_CHECK)
            thisline$ = TRIM$(thisline$)
            isSF = 0
            ncthisline$ = UCASE$(thisline$)
            IF LEFT$(ncthisline$, 4) = "SUB " THEN isSF = 1
            IF LEFT$(ncthisline$, 5) = CHR$(3) + "SUB " THEN isSF = 1
            IF LEFT$(ncthisline$, 9) = "FUNCTION " THEN isSF = 2
            IF LEFT$(ncthisline$, 10) = CHR$(3) + "FUNCTION " THEN isSF = 2
            IF isSF > 0 THEN
                IF RIGHT$(ncthisline$, 7) = " STATIC" OR RIGHT$(ncthisline$, 8) = " " + CHR$(3) + "STATIC" THEN
                    thisline$ = RTRIM$(LEFT$(thisline$, LEN(thisline$) - 7))
                END IF

                thisline$ = TRIM$(thisline$)
                checkargs = INSTR(thisline$, "(")
                IF checkargs > 0 THEN
                    sfname$ = RTRIM$(LEFT$(thisline$, checkargs - 1))
                ELSE
                    sfname$ = thisline$
                END IF

                'It could be that SUB or FUNCTION is inside a DECLARE LIBRARY.
                'In such case, it must be ignored:
                InsideDECLARE = 0
                FOR declib_CHECK = currSF_CHECK TO 1 STEP -1
                    thisline$ = GETLINE$(declib_CHECK)
                    thisline$ = TRIM$(thisline$)
                    ncthisline$ = UCASE$(thisline$)
                    IF LEFT$(ncthisline$, 8) = "DECLARE " AND INSTR(ncthisline$, " LIBRARY") > 0 THEN InsideDECLARE = -1: EXIT FOR
                    IF LEFT$(ncthisline$, 9) = CHR$(3) + "DECLARE " AND INSTR(ncthisline$, " LIBRARY") > 0 THEN InsideDECLARE = -1: EXIT FOR
                    IF LEFT$(ncthisline$, 11) = "END DECLARE" THEN EXIT FOR
                    IF LEFT$(ncthisline$, 13) = CHR$(3) + "END " + CHR$(3) + "DECLARE" THEN EXIT FOR
                NEXT

                IF InsideDECLARE = -1 THEN
                    sfname$ = "MAIN MODULE"
                END IF
                EXIT FOR
            END IF
        NEXT
    END IF

    IF ASC(sfname$, 1) = 3 THEN sfname$ = MID$(sfname$, 2)
    CLIENT_CURRENTMODULE = sfname$
END SUB

'------------------------------------------------------------------------------
FUNCTION FIND_SYMBOL (Start, BaseString$, SearchTerm$)
    'Works line INSTR, except it looks for SearchTerm$ outside quotation marks.
    Found = INSTR(Start, BaseString$, SearchTerm$)
    IF Found = 0 THEN EXIT FUNCTION

    FOR i = 1 TO Found
        IF ASC(BaseString$, i) = 34 THEN InQuote = NOT InQuote
    NEXT i

    IF NOT InQuote THEN FIND_SYMBOL = Found
END FUNCTION

'------------------------------------------------------------------------------
SUB READ_KEYWORDS
    RESTORE QB64KeywordsDATA
    'Populate QB64KEYWORDS():
    DO
        READ ThisKeyword$
        IF ThisKeyword$ = "**END**" THEN
            EXIT DO
        END IF
        TotalKeywords = TotalKeywords + 1
        REDIM _PRESERVE QB64KEYWORDS(1 TO TotalKeywords) AS STRING
        QB64KEYWORDS(TotalKeywords) = ThisKeyword$
    LOOP
END SUB

'------------------------------------------------------------------------------
FUNCTION IS_KEYWORD (Text$)
    uText$ = UCASE$(TRIM$(Text$))
    FOR i = 1 TO UBOUND(QB64KEYWORDS)
        IF QB64KEYWORDS(i) = uText$ THEN IS_KEYWORD = -1: EXIT FUNCTION
    NEXT i
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION SHOWMENU (MenuSetup$, MenuID$, mx, my)
    TYPE MenuType
        Caption AS STRING * 50
        Highlight AS _BYTE
        Inactive AS _BYTE
        Y AS SINGLE
    END TYPE

    'Color constants
    CONST MenuBG_COLOR = _RGB32(190, 190, 190)
    CONST MenuBorder_COLOR = _RGB32(0, 0, 0)
    CONST InactiveItem_COLOR = _RGB32(150, 150, 150)
    CONST ActiveItem_COLOR = _RGB32(0, 0, 0)
    CONST ActiveItemSelected_COLOR = _RGB32(255, 255, 255)
    CONST Highlight_COLOR = _RGB32(0, 80, 80)

    DIM MenuH AS SINGLE

    IF LEN(MenuSetup$) = 0 THEN EXIT FUNCTION
    IF mx < 0 THEN mx = 0
    IF my < 0 THEN my = 0

    WHILE _MOUSEBUTTON(2): mi = _MOUSEINPUT: WEND
    REDIM Choices(1 TO 1) AS MenuType
    TotalChoices = 1: Separators = 0
    MaxLen = 25
    IF INSTR(MenuSetup$, CHR$(LF)) = 0 THEN
        IF LEFT$(MenuSetup$, 1) = "~" THEN Choices(TotalChoices).Inactive = -1: MenuSetup$ = MID$(MenuSetup$, 2)
        CheckHighlight = INSTR(MenuSetup$, "&")
        Choices(TotalChoices).Highlight = CheckHighlight
        Choices(TotalChoices).Caption = LEFT$(MenuSetup$, CheckHighlight - 1) + MID$(MenuSetup$, CheckHighlight + 1)
        IF LEN(RTRIM$(Choices(TotalChoices).Caption)) > MaxLen THEN MaxLen = LEN(RTRIM$(Choices(TotalChoices).Caption))
    ELSE
        Position = 0
        DO
            Position = Position + 1
            IF Position > LEN(MenuSetup$) THEN EXIT DO
            ThisChar = ASC(MenuSetup$, Position)
            SELECT CASE ThisChar
                CASE LF
                    Choices(TotalChoices).Caption = TempCaption$
                    IF RTRIM$(Choices(TotalChoices).Caption) = "-" THEN
                        Separators = Separators + 1
                        Choices(TotalChoices).Inactive = -1
                    END IF
                    IF LEN(RTRIM$(Choices(TotalChoices).Caption)) > MaxLen THEN MaxLen = LEN(RTRIM$(Choices(TotalChoices).Caption))
                    TempCaption$ = ""
                    IF LEN(MID$(MenuSetup$, Position + 1)) > 0 THEN
                        TotalChoices = TotalChoices + 1
                        REDIM _PRESERVE Choices(1 TO TotalChoices) AS MenuType
                    END IF
                CASE 126 '~
                    Choices(TotalChoices).Inactive = -1
                CASE 38 '&
                    Choices(TotalChoices).Highlight = LEN(TempCaption$) + 1
                CASE ELSE
                    TempCaption$ = TempCaption$ + CHR$(ThisChar)
            END SELECT
        LOOP
        IF LEN(TempCaption$) > 0 THEN Choices(TotalChoices).Caption = TempCaption$
        IF LEN(RTRIM$(Choices(TotalChoices).Caption)) > MaxLen THEN MaxLen = LEN(RTRIM$(Choices(TotalChoices).Caption))
    END IF

    MenuW = (MaxLen + 6) * _PRINTWIDTH("W")
    IF _WIDTH - mx < MenuW THEN MenuX = _WIDTH - MenuW ELSE MenuX = mx
    MenuH = ((TotalChoices - Separators) * (_FONTHEIGHT * 1.5)) + Separators * (_FONTHEIGHT / 2)
    IF _HEIGHT - my < MenuH THEN MenuY = my - MenuH ELSE MenuY = my

    _KEYCLEAR
    DO
        _LIMIT 30
        GOSUB DrawMenu
        WHILE _MOUSEINPUT
            IF _MOUSEWHEEL THEN EXIT DO
        WEND
        mx = _MOUSEX
        my = _MOUSEY
        mb1 = _MOUSEBUTTON(1)
        mb2 = _MOUSEBUTTON(2)

        'Hover highlight:
        IF mx <> prev.mx OR my <> prev.my THEN
            prev.mx = mx: prev.my = my
            SelectedItem = 0
            FOR i = 1 TO TotalChoices
                IF RTRIM$(Choices(i).Caption) <> "-" THEN
                    IF mx >= MenuX AND mx <= MenuX + MenuW AND my >= Choices(i).Y - (_FONTHEIGHT / 3) AND my <= Choices(i).Y + (_FONTHEIGHT * 1.5) THEN
                        SelectedItem = i
                        GOSUB Highlight
                        EXIT FOR
                    END IF
                END IF
            NEXT i
        ELSE
            i = SelectedItem
            GOSUB Highlight
        END IF

        _DISPLAY

        'Check for click
        IF mb1 AND mb1held = 0 THEN mb1held = -1
        IF mb1held = -1 AND mb1 = 0 THEN mb1released = -1: mb1held = 0

        'Check for mouse up:
        IF mb1released THEN
            IF mx = _MOUSEX AND my = _MOUSEY THEN
                CheckForClick:
                FOR i = 1 TO TotalChoices
                    IF mx >= MenuX AND mx <= MenuX + MenuW AND my >= Choices(i).Y - (_FONTHEIGHT / 3) AND my <= Choices(i).Y + (_FONTHEIGHT * 1.5) THEN
                        ForceCheckForClick:
                        IF i = 0 THEN
                            'Enter is pressed while no choice is highlighted
                            PCOPY 1, 0
                            EXIT FUNCTION
                        END IF
                        IF Choices(i).Inactive = 0 THEN
                            IF LEN(MenuID$) THEN
                                SHOWMENU = CVI(MID$(MenuID$, i * 2 - 1, 2))
                            ELSE
                                SHOWMENU = i
                            END IF
                            PCOPY 1, 0
                            EXIT FUNCTION
                        END IF
                    END IF
                NEXT i
                IF mx < MenuX OR mx > MenuX + MenuW OR my < MenuY OR my > MenuY + MenuH THEN
                    'Click outside menu boundaries
                    PCOPY 1, 0
                    EXIT FUNCTION
                END IF
                mb1released = 0
            END IF
        ELSEIF mb2 THEN
            IF mx < MenuX OR mx > MenuX + MenuW OR my < MenuY OR my > MenuY + MenuH THEN
                PCOPY 1, 0
                EXIT FUNCTION
            ELSE
                GOTO CheckForClick
            END IF
        END IF

        k = _KEYHIT
        IF k = -13 THEN i = SelectedItem: GOTO ForceCheckForClick
        IF k = 20480 THEN 'Down arrow key
            DO
                SelectedItem = SelectedItem + 1
                IF SelectedItem > TotalChoices THEN SelectedItem = 1
                i = SelectedItem
            LOOP WHILE RTRIM$(Choices(i).Caption) = "-"
            GOSUB Highlight
        ELSEIF k = 18432 THEN 'Up arrow key
            DO
                SelectedItem = SelectedItem - 1
                IF SelectedItem < 1 THEN SelectedItem = TotalChoices
                i = SelectedItem
            LOOP WHILE RTRIM$(Choices(i).Caption) = "-"
            GOSUB Highlight
        END IF

        'Check hotkey presses
        IF (k >= 65 AND k <= 90) OR (k >= 97 AND k <= 122) OR (k >= 48 AND k <= 57) THEN
            FOR i = 1 TO TotalChoices
                IF UCASE$(CHR$(k)) = UCASE$(MID$(Choices(i).Caption, Choices(i).Highlight, 1)) THEN
                    GOTO ForceCheckForClick
                END IF
            NEXT i
        END IF
        IF _EXIT THEN USERQUIT = -1: EXIT DO
    LOOP UNTIL k = -27

    _KEYCLEAR
    PCOPY 1, 0
    EXIT FUNCTION

    DrawMenu:
    PCOPY 1, 0

    IF ShowPauseIcon THEN
        PauseFadeStep# = TIMER - SetPause#
        IF (PauseFadeStep# <= .75) THEN
            PauseIconBar.W = 30
            PauseIconBar.H = 100
            PauseIcon.X = _WIDTH / 2 - (PauseIconBar.W * 2.5) / 2
            PauseIcon.Y = _HEIGHT / 2 - PauseIconBar.H / 2

            LINE (PauseIcon.X, PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
            LINE (PauseIcon.X + (PauseIconBar.W * 1.5), PauseIcon.Y)-STEP(PauseIconBar.W - 1, PauseIconBar.H - 1), _RGBA32(0, 178, 179, 255 - (340 * PauseFadeStep#)), BF
        ELSE
            ShowPauseIcon = 0
        END IF
    END IF

    LINE (MenuX, MenuY)-STEP(MenuW, MenuH), MenuBG_COLOR, BF
    LINE (MenuX - 1, MenuY - 1)-STEP(MenuW + 2, MenuH + 2), MenuBorder_COLOR, B
    COLOR , _RGBA32(0, 0, 0, 0)
    FOR i = 1 TO TotalChoices
        IF i = 1 THEN
            Choices(i).Y = MenuY + (_FONTHEIGHT / 3)
        ELSE
            Choices(i).Y = Choices(i - 1).Y + ((_FONTHEIGHT * 1.5))
        END IF
        IF AfterSeparator THEN Choices(i).Y = Choices(i).Y - (_FONTHEIGHT)
        IF RTRIM$(Choices(i).Caption) = "-" THEN
            LINE (MenuX, Choices(i).Y - (_FONTHEIGHT * .1))-STEP(MenuW, 0), MenuBorder_COLOR
            AfterSeparator = -1
        ELSE
            IF Choices(i).Inactive THEN COLOR InactiveItem_COLOR ELSE COLOR ActiveItem_COLOR
            _PRINTSTRING (MenuX + (_PRINTWIDTH("W") * 3), Choices(i).Y), RTRIM$(Choices(i).Caption)
            AfterSeparator = 0
        END IF
        IF Choices(i).Inactive = 0 AND Choices(i).Highlight > 0 THEN
            LINE (MenuX + (_PRINTWIDTH("W") * (Choices(i).Highlight + 2)), Choices(i).Y + _FONTHEIGHT)-STEP(_PRINTWIDTH("W"), 0), ActiveItem_COLOR
        END IF
    NEXT
    RETURN

    Highlight:
    IF i < 1 OR i > TotalChoices THEN RETURN
    IF RTRIM$(Choices(i).Caption) <> "-" THEN
        LINE (MenuX, Choices(i).Y - (_FONTHEIGHT / 3))-STEP(MenuW, _FONTHEIGHT * 1.5), Highlight_COLOR, BF
        IF Choices(i).Inactive THEN COLOR InactiveItem_COLOR ELSE COLOR ActiveItemSelected_COLOR
        _PRINTSTRING (MenuX + (_PRINTWIDTH("W") * 3), Choices(i).Y), RTRIM$(Choices(i).Caption)
    END IF
    IF Choices(i).Inactive = 0 AND Choices(i).Highlight > 0 THEN
        LINE (MenuX + (_PRINTWIDTH("W") * (Choices(i).Highlight + 2)), Choices(i).Y + _FONTHEIGHT)-STEP(_PRINTWIDTH("W"), 0), ActiveItemSelected_COLOR
    END IF
    RETURN
END FUNCTION

FUNCTION WASP_IMAGE&
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000031\iSh3_T[5>dbMC"
    A$ = A$ + "h4;fTLC[EC000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000\Ta[[QA3WnSoX[h;VC\Jc8=\B=TbH:di9CY?7c<"
    A$ = A$ + "Q^L0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000Ba<l4]Ta[k8AnB^Rl@;g:JcZD[h<Un\R_0jb=jRW9Cf;IBl200000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000`E?;?CBY\k<^DaY[h@l:^Rk4Kf:JCZA[8<P^LR]T9a<ZbToj7;Bf[500000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000`ecbgcF?;_SEYLk:bd`W[8Ak6^Rk4;f:FCY?[H;H>\RX\8^:B2QaFh5PY8800000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000S5mlQ@fd"
    A$ = A$ + "eoXG@;_RF]\k:jDaX[8Ak2^Rl0ke::cW9[X:?^[RU4X[:6bMTJX4>=g700000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000`IB?o1^Mmm2R6ee[HH@;_RJa\k"
    A$ = A$ + ":25aY[XAk2^Rid:e:2SV4[8::F[RShgZ9faJFZ88`Q9600000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000NKW?FcU=n:bVef[8IA?_RLa\k:6EaX[XAjn]R"
    A$ = A$ + "i\Zd:jbU3[8:8B[RShgZ9BQEmUH7YmH300000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000lfc]oAOMW_SfY=n927fg[hIB?_RMalk:6e`X[8Ahj]RiP:d:jbU1[h9"
    A$ = A$ + "7:[RS\gY;f@?NmW7[1Y000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000D1;d1Hhgk3GPM[oRjY=n::gef[hIC?_RNa\k:6e`W[8Aif]RgHjc:jBUoZX932KRO47WDn@A"
    A$ = A$ + "Ue50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000hM=m:0Wec;RG?3O?Ca\k;U4b\CE=oFnFU@KgRea[H?f4M^LF=dX_;]QV6KS5?R;8C@XZ:00000000000"
    A$ = A$ + "0000000000000000k5]kXX8hlg8QMW_RlY=n::Wee[HJC?_RMa\k:nT`V[h@e^]Rf@Zc:^bTnZX946;RF\UPT2QB^m2000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000;bMmB4Xf"
    A$ = A$ + "eO5NHGoO`MMm7NVeeGHGA;?QCi<l3VDb];h?7_NPh8<j16C_V;h:i:NQTlJf;faX>3i4?VkS=D7Woi`H9NE5WQh:UX7W60000000"
    A$ = A$ + "0000IN^oWeHhl[HQN[_RlY=n:B7ff[8JB?_RK]\k:fD`V[H@e^]RdDJc:^BUoZh9:J;R<D4KW2bMOf0000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000009gdKaRKGoO9bMm3>hff;8OJO_PdU]m"
    A$ = A$ + "2^Vef;8HDG_PG9ml2bTc`;8A;o^Ph@Lj263`U;8:f2^PThJf12bYC7h6MZ<PG0i_5:QNW2i2EYGT4\2?dmaE\5cGY>l5KJ^o:jXh"
    A$ = A$ + "l[HQMW_Rm]=n:6gee[8IA;_RIULk:bD`U[h?bV]ReHZc:fBV4Wh842[U8P4Mo100000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000CbmlJ=Igd;HTNK_P>j=n2Rhgi;hPNW_PjeMn2nffi;hIIW_P"
    A$ = A$ + "LImm26Edc;HA=7_PlTLk2>C`X;h:i6^PSd:f2faW>;H6C>\PDP8^1:AO_nG4dIjP<H5OL>08\lID0nhRGnMm:6ihlSHQNW_RjU=n"
    A$ = A$ + ":nfee[hH@;_RFQ<k:V4`U[h?gb]Re\jd:63Y=SH5^a9]B@VTk000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000PUJ7oHInMm1Z9hg;8VPS_PE6Nn22Ihk;XRPc_P32>o2VGgk;8LMc_PUU]n2VUeh;HC"
    A$ = A$ + "CG_P2aLl2R3aZ;h;l>^PTlJf2jAX>;86C6\PD@H]26aMX:84[YIP?LfUnU@AV]94e@TeHmhW_29hjS8QN[ORi]=n:fVef[8H@;_R"
    A$ = A$ + "CM<k:R4`V[h?gj]RghZe:nRX=cX2HYH]R@8\;000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000\9gb_cWOC?QP2^m2nIhg;XWS[_PKB>o2N9im;XTTg_P;:^o2>8hn;XNOk_P`e]o2FVfl;XEEW_P<m<m2jca"
    A$ = A$ + "];X<oJ^PX4kf222YA;X6FB\PEHX]2:aMX:84_eYP?@6U16AIDJh5J=7fKHdEI3g^CWiPO_oQdU=n:VFee[hF>7_RAM<k:ND`V[X?"
    A$ = A$ + "hn]Rg0ke8fAQeNJ4UEYM00000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "Nj=m74ZgcKGYRK_PU:>n2J:ij;HYVc_PRJ^o22Jio;XVRo_PE6no2jhgo;HQMo_Plemo2n6gn;hHKg_PEMMn2JTcb;8>2W^P]L;h"
    A$ = A$ + "2BRZE;87MV\PGh8_2FAP`:H4c=ZP@X6V1RaNV:i<UR<bD45JO;TOER;PN_oQ`Qmm:>Vdd[8F=7_R>Q<k:B4`V[h>ff]Rd\:e9^0H"
    A$ = A$ + "CJ\6dA::00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000T2>mMPJhe;XZ"
    A$ = A$ + "SS_P\Jnn2^jil;hZWk_PZJno2Njho;8YQo_PO2no2Zigo;hTMo_P8^mo2fgfo;8LKo_PO]Mo22edg;8@8k^Pdd[i2Z2\J;X8Tn\P"
    A$ = A$ + "LDi`2RQRi:H5meZPDPgY0:BTlZ9>_B]aUl7XBOBGc]LNIG?RZE]m:j5dd[8E;o^R9E\j:n3_S[8>`VMRRdh^OR`D4Bj70fj00000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000Y:^mYhjhg?8\VW_PaN>o26Kj"
    A$ = A$ + "n;H\Wo_P`Fno2n:io;hZRo_PZ:no2BJho;HWOo_PEfmo2Zhfo;8OKo_P]a]o2fEfk;hB??_Pm@\j2:C^R;X:_R]PS8Zc2faU3;X6"
    A$ = A$ + "<^[PJ@H]0B2UoZ9?^J]a\lX]>cQDWAmKGGORSA]m:Vecb[XC9g^R49li:^3^O[8=\F=R<8FU47AH@n4000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000`B>nQ<[iic7]XcoPeZ^o2N;jo;h]Vo_PfJno"
    A$ = A$ + "2FKio;h\Uo_P`Bno2fjho;XYQo_PM2no2FIgo;XQKo_Pj]mo2bFgn;XFGS_P;eLl2jS`Z;8=i6^P\hje2FBY?;H8HF\PN0i_0N2U"
    A$ = A$ + "5?i>\F=a]@Y^>;2HialJHWoQL5=m::5ca[8B6_^Ro`kh:N3\JWh8?j;W7dTOojaNXn0000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000aNnn?LKjl[V^ZkOQjRno1Zkio;h^Wo_PkNno2Zkio;H^Vo_P"
    A$ = A$ + "fFno2:Kio;H[So_PW6no2jigo;8UMo_P7^mo2VGgn;XJKc_PMI=n2b4c`;H@4W^Ph\Kh2nb[H;H:W:]PS\Ib1FBV8WX=\Jm]I\FS"
    A$ = A$ + "MCcPPN;IGSoQG1ml:bDb^[8A2S^RjLkg:>cZESH3Q=I`@XeQc100000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000fR>o4XKjnKD_Xo?QoRno22Ljo;8`Xo_P0Sno2n;jo;X_Xo_PlNno2R[io;h\"
    A$ = A$ + "Uo_P]>no2RJho;XWPo_PBfmo2N8go;HNMk_PZa=o2bEeg;XC<o^P25<j2R3^P;H<_N]P\P:d1BRV7o79F:\V6dcFRG5_NOYFCKoR"
    A$ = A$ + "@aLl9JDa[[H?l>^Rg4[f9:2SlbY18E7cELVTO000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000d;jnoA`YoOL2Wno5>Ljo;8aYo_P3Wno2BLjo;h`Yo_P0Wno2f;jo;H^Vo_PdFno2j:i"
    A$ = A$ + "o;hYQo_PO2no26Igo;HQLo_Pie]o2RVfj;hFDC_P>YLk264`U;h=eb]P^Xjd2V2X<oW4S]8YB44Fc^UaS;6E??OS;Y\k:6D`W[H>"
    A$ = A$ + "ej]RbT:e9b`F<:<24mFR00000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000LjnC0aZkO?7[no0RljoC8b[oOP7[no2N\jo;XaZo_P4Wno26Ljo;X_Xo_PjNno2FKio;H[So_PV6no"
    A$ = A$ + "2bigo;HTMo_P5fmo2NWgm;XIHS_PHmLl2VDaY;8?k6^PaT:e2NBV7?H1g<5eL\ELWTd^MoAC<7OL5Alj>bc^SW8=_VMRPTH^MJ@A"
    A$ = A$ + "aml4NQX;00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000PaZko3:_noEa<koC8c\ooP;cno2^<ko;Hb[o_P8_no2F\jo;h`Yo_P0Sno2Zkio;8]Uo_P^>no2BJho;8VNo_P"
    A$ = A$ + ">fmo2jWgn;HLL[_PMm\l22ea[;H@l6^Pddje0NAMOJZ0W\CU00000TcY;C@@l6n@01li;RC]N[H<Y>MR:H5R2O0@ZiI6_UI00000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000:_noJlLkogec]o?Q>gno2f<ko;Hc\o_P;_no2Vljo;XaZo_P3Wno2n;jo;X^Wo_PdFno2^Zho;HXOo_PDjmo2FHgn;8M"
    A$ = A$ + "Lc_PS5ml2:ea[;8@in]PaLJd3J@@V9M7P]g:0000000000`>ffM9l\kh3F3\IWH702kX6dCJA71ElQ3000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000@3iPE3?4dHllP@T=W4@MWXDLEPgBaE"
    A$ = A$ + "0F<5I=8bCDeNl:ADhUJ49a6R?84IGeP>IUB3iPE1000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0hLko[Ad^oOFAkno3:]koC8d^o_P?gno2b<ko;Hb[o_P6[no2:Ljo;X_Xo_PhJno22;io;HYQo_PHjmo2Rhgo;XML__PTmLl22e`"
    A$ = A$ + "X;H@ij=PL08[R>0;7m::kYi00000000000P:C^k4gLkgmmRYBc82:IGa7X3GS:AEk90000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000008QCb5`319F=C0EMNN1I@2N5LE8aA`TL76aB_5e305V<>`3GVlP@UIR3nle;@HDJ"
    A$ = A$ + "51AAWQF49efVC8eN3;1Dgil30=fU<DcD4]P<>I00000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000@d]oo4"
    A$ = A$ + "Dono:=mko_Wd^oOQAkno2jLko;8c\o_P:_no2B\jo;8`Yo_PjJno22;io;XYQo_PJnmo2Vhgn;XML[_PR1Ml22e`V;8?bZmP9PeP"
    A$ = A$ + "93QAOI6000000000000000097fZ3e4;giUALN^Z1c<5d<l3H1100000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000`305f<ETeOhNAI@V]4;i6M>`cFO00000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "<H3E>dP>JU33e<eM;03CTVP9m8G2Y4d700000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000@gno8<mko7Ce"
    A$ = A$ + "_oOJCono5:]koGhc]oOP;cno2N\jo;X`Yo_PkNno26;io;XYQo_PJnmo2V8hn;XMJS_PR1mk2nT`T7H:JB<V5HTKCcAGie200000"
    A$ = A$ + "0000000000P7faI4^@jc4J0<=Ul1\DTZ><DH4000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000P6GQg2@L4J9VAK"
    A$ = A$ + "HBN47Y6K@DTI:000000000000000000000@4;mF0000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000;h2BMT@:0eD2V\C?9LB?5000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000Pd^oo5Eono6=mk"
    A$ = A$ + "oGGd^o_Q=cno3Rljo;X`Yo_PkNno2:Kio;XYQo_PHnmo2VHhn;HMIK_PRa<k2jd_R384fQ:^5`3G3KRLB>100000000000000009"
    A$ = A$ + "7fj6C8ULcJ@8dll2a\d@00000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000Y47UMHaG6Rl4CQGZJ85MH@1BYM`315F=AT4K"
    A$ = A$ + "a=1Dd9J5K=X_FdeQ=GQG8:=5JA8cC8UNf6AB\Ii30A6G>\3GT000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000lb]k2<lheoAd^o?D>cno"
    A$ = A$ + "hQljoKX`YooPjNno16;io;HYQo_PGnmo2N8hm;HMHC_PR]lj1b3[D?i1X]9b4HCDZ6bJ<R0000000000000000@7bMIA6\Q:>SP8"
    A$ = A$ + "dL:3g0e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000P:"
    A$ = A$ + "A^k7gdY`GQQO]bG7HB<RO@jc3FR[Gce:^Z]93UZc1DSQYN26V59gLLUN`iQDbUB49aFSH@VSLKaH<Fm4@=WW@D4JU1AAW9d3196<"
    A$ = A$ + ">`cFWh0?Keb32AV??<TIO11B[Mi4EiWcDP5P9o0AVa63e8E50000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000000000000000000@\IoN1k:nmRL\jooD`YooM"
    A$ = A$ + "iJno62;ioC8YQo_PG6no1Fhgk;hLD;_PU]\j0fQT1oJ2T=Yb403BJVAGmA000000000000000002SDcQ7Xa9B_@<7i3000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000`6jIZ<Udh^bR5\>GO4YIioR@9`ocRZ"
    A$ = A$ + "Do?>n>no7alko?EehS?Bk2?`NHgYMc`=E]V4=5gXF<VSL7AAXI88C=G;PHeM5000000000000000000000000000000000000000"
    A$ = A$ + "000000000h`?NA@3l`e;@PDJ@JaG9b=45QVR=XCFE00000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000LJfb?P[OS?8hJno<ijhoCWX"
    A$ = A$ + "PoOQE:^o3Bhgj7hLC7OPG=Li8b@Of2L2JMHc4Pb?B6QC\A00000000000d0@L112M\b^80B<R:aBX90000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000P5\Ii0KTgYiEQN\N?2k1ko;@ENoCAKIn_7:VkoV4:co;c]Moo@7_noJMmm"
    A$ = A$ + "oSWgoo?SJc_n9mIcbCBJBRG?jQi2000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000?4DHF4QB^=H5JE8f>dCGSa0=@=000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000JVMm2\ifh_1VM_oAC:^ocA8g"
    A$ = A$ + "gKXMD7oPkl:fM^`M`><2Cegd3LB>Ef0APA00000000000d@==Qd1KPReB@dGb00000000000LD6S;daJBj17Y199JH6SJTQH6V00"
    A$ = A$ + "00000000000000000000FdfU2TaMV:I2Paho7HfUoObPWno23EfoD`6Vo[AP`no8G>lo]dZeogS`VooD?7oo^i=oo_hgnoOZSooo"
    A$ = A$ + "iULl67EY:C100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0``=C]A46U6W@HTJgbP=Ca100000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000PCPQVQ>46JA@@DHki0?GEi49IV^@<TG3g@?FmK2aPd^"
    A$ = A$ + ":0cAXV0<7IX=jY9BhhWWD000000000000000000000000000000000000000000000000000000O>kn0dYLjM8Hfe_TL@k>NNPYb"
    A$ = A$ + "ib0M[>l191gd342<Qj0AOY000000E8EL2X`:0aI3h<UXMTgXf]aLPZK6[E9iGheQhC1Eja_4>=7mA`TKO31AWAJ6PEH>00000000"
    A$ = A$ + "0006_aIM6h4LoCAMPn?:=VkoQleNoc@AVmo4T1ioHLGYok1Sjn?:TflogT[go_TaYo?IDCoon1nno[9inoo\WkoofA\jnRUZ=S00"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000@3hLU>"
    A$ = A$ + ">\3Gg^P;8Y400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000k@HYYl2PSb7<3JJbaHHZgWbMKno0U`co40cAog@>AmO4mDeo@X3DoK`;4m_0RDcoJ@ULocbOSnO:"
    A$ = A$ + "iaYlW87V`nBMG^T?oaI100000000000000000000000000000000000000000000000000000@dVnB`?N>l<=PX_Fb0LVBl14IVe"
    A$ = A$ + "2\a9_BQEeYR7f5:LNPGYG7RP_j_8;JkoU0I_oO2V4o?:KRloWTIao7BQan?6W1ioC<UNo7aBaao33E6VGPVTeX`B[E?5\9ioJLGY"
    A$ = A$ + "o;bQdnO3mDeo<8dHo71G6n_5`]ioL0X[oCRV4o?<\>mo1];hoGUbYooKD7oo;n=no7Zilo_]ZgooR1<hl1000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000000000000000000000003e852;03ClYP:1M60"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000jh7W"
    A$ = A$ + "khBOOJk;o9:n`8XYoCcQZn?=;fjoY1Z]o7Ka=oo_Fgmo8k=io[lgUo?cQOno7_Mhooh\1oO<7Zjo^08Yo[2OPnO:haioX<gUoGbK"
    A$ = A$ + "CV<;_1i>0000000000000000000000000000000000000000000000000000000000000000FLgYV0AKPB:219Fl7\b?lo1NOn_:"
    A$ = A$ + "Cjko[Pi`ooBX;o?=Z:moiD;goWc]NoO>k6nof\kho?3^QoO:Q^loJ<WWo;ADjmO4I5hl9LTGoWQLQn?6b5joLTgYoSP;2mo2nheo"
    A$ = A$ + "?<eNo?AI?n?6e5joO\h]oWBX9oo=_Bmo9e[gooeaVooMDgno?^Mlo;Jhdo?VO3Ol@9;cM0000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000009T2@fTP9kX50000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000V0UIQDRI6^j;ji9ob4hXo_CRZn_CHJko"
    A$ = A$ + "Xaj`o_5YonoHB>jo7eVPooRFcmO8CefoJ`TIooQCWmO=L9go4R[booG^<o_HW2lo39I\o7cOSn_:iaioX<gUoK2LDVO6?]fQ[DeJ"
    A$ = A$ + "7000000000000000000000000000000000000000000000000`AJ>VB6U]HZ]Di_gKRSin?3:Yfo=0c@oGRNNno?d^mooD;go[RU"
    A$ = A$ + "0o?89Jko0a[ho;edgooCCSoo85]moocbaoo<nJnoTXiaoW0@JmO5RUhoFXfUoOQKLn_6`Yio7@B=o[`=Cm_3:mfo?PEPoK1KEn_<"
    A$ = A$ + ">>komli`ok2X6oo>^2mo>U;foCVaPooM;Cno=:mio;jf]oo@G:[P000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000:`BA<0000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000LA><Qe2\dCka`7XoccQXn?GOZko6Zkbo7:bFooX;OmoQeI]oC@;8m?2"
    A$ = A$ + "gDeo9\cFoW0?MmO2ldeo9`CGooAGlm_U7Smo]2=go;[dNo_X:Smof9kaoO4Uano;n1joQ4FPoG06Tl?2QlB\X4eI<00000000000"
    A$ = A$ + "0000000000000000000000RMMNP4FQgN3lBBdG1H4n_?^>mool:eo7D\HoO6PigoGh3DoO2K8n_5e1joB<gWoO2Q[nO7nUkoPMll"
    A$ = A$ + "o[Feoo?HCoooEQ]ooGDdfo?:LFlo7@cAoKQIDnO5V=ioFTfUoWQJCn_1Odbo9hBAo;1C]m_HK6konZ]ho;]i\oOeXknohfmio7cV"
    A$ = A$ + "onO?VRlo?A;do;6_Do?M1Smo7V<goCFYlJmB=J:1000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000@5eLdM6`1:lcaBRmOELNko5RKbo3jaDoOU3;moh9;bo36Z3o?:ZYho9XSFo[0@Smo27Yfo;PTKo[PB"
    A$ = A$ + "_mO26]fol89]ooTY5o?G\RlodI[coOYaGoo[BgmoPR\eooEXin?_?GmolLeHoCP5Rl?2S4C[V0eI600000000000000000000008"
    A$ = A$ + "]9I69<dIoF0@Tm_1Bego4L5Ro3cX=ooD4KnoGILjoS5b\o_4FMgoY0UGo_4Jem?3mTeo5\5RogCU6oO16=jo@e:ioOGeooOIBooo"
    A$ = A$ + "EYmookaMKnO33meoDheRoGAH?nO5U9ioK`6UoK@7[l_2[hcoWai[oc\gUoOcQKno@;>jo;]i[oo\E3noV@HZooBSbno>Nbko:9J`"
    A$ = A$ + "o[5[6o_JeZlo]iZ`jCCK4N200000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "Bhb?2J@7Zho2S4coTMiZo_Y`Ao?TonloXUJ`o_DVfn_@DBko4YY^oG1Dbmo22Ifo;XDLoc@DimO3G1ho<LUPoG1I<n?D\flo<UJb"
    A$ = A$ + "oWTY7o?BRBlo>E:aoOF\:o?R0;moIDdGo?4JimobJomoImfNo?`5Slo4b@4R0000000000000000RXFSSPP@TaM1;=go5hUTog@Q"
    A$ = A$ + "gn_;Ynlo0I[foSfc[o_H8OnoL8WUoK0=8mo0k`eoWQHUokdL0nO1J=hoj89_ogQKjn_NAWooiImoo_6eoo?GKkooEPEMo7ADgmo4"
    A$ = A$ + "LIhoDdERoCAH>n?7^Mio7l1;ocP:hloF?6joZZldo_KeMo_[>Wmoo9;aoO2LBn?7[1ioS@WVocRPTno=:Zjo3AI\oo4Wen?GRRko"
    A$ = A$ + "bPFO;10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000000000000000008Q;0YV1JPRo@H2=o7:^0o_U"
    A$ = A$ + "ojloaaZ`oWDUcnO?=jjonlH\o7TTcnOAHVko=LDJoc0CbmO3Bago>`ERok@G<nO3Q1io]Lh[oW5]AooD_nloAiJcokdZ;oOBVJlo"
    A$ = A$ + ":AJaoO2LDn?2eHeo703CocA@GmO]:7moT]gQoOABUU_:\i8?00000PbJ:J120mEg5TTLoCPIGn??V^lo7N=kooWfdo?QMKooNA]l"
    A$ = A$ + "ok2Ten_0Ghao28B;oK`Dimo1Dego=`GZoc1TgnO>Lflo^A\jocGfooOLHoooVQMooK5eioO4;QfoADUOo;aE0no4KEhoDd5Ro[AJ"
    A$ = A$ + "Dno2[\co6X19o;1=6mo9L=goZ0FNoc1E`m?47IfoBdDKoKaEfmo6NmgoPPFRoSBL@n?<iIiojdGVoCDRQn?7odTH000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000006l@e<6\1:dk09bl?Yi6loj`EKo75VfnO?<fjol`X[ogSS"
    A$ = A$ + "_n?@B>ko4IY]oocS`nO2m8fo=hDMog`E2n_3Maho>HfTogPIEn_ATJloKE[doS5]AoOE`jloAeJcokdZ;o?AN2lo;XTLo[P@WmO2"
    A$ = A$ + "lheo8@cDok0==mOLVVko_hWXo[BMHj<<^]h1:4dG[BP=Dmo0?Ugo<]:bokHfZo_JB;ooZI]moKCa`o_1cVnod8JboW1BJm_2ZLco"
    A$ = A$ + ";X3CoOAKAnO;G^kobLJbo_b[?oOSM?ooRFnno77fgo_E>3oo;Iljo7QCbm?4AQgoA<eNo;AEom_4I=hoF4VRo?Q@Im?1BTao5L18"
    A$ = A$ + "oO@8alO2Zhco:l2Aoc@=>m_3kDeo@8DGoGABTm?6?]foMH5Mo;2GhmO:Uigo``6QoOa=5M600000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000WLEL7PQAQ]l1JPboGnj]o_bAGm?3aLdomd8\ocSS`n_?@:ko05i\o;dTenOADJkoe0XX"
    A$ = A$ + "o[0?Nm?39mfo=DUOog`E4n_3Paho=l5So3E[<ooEbnloF1[co?E[<o_D[ZloBe:co;2LHn_2:Ago;XDLo[0A[m_2o@foK@EMocG^"
    A$ = A$ + ">o?CKVko`48Yo3BI7^i0R@So30CCoCSOKnOS?gmo[]lioSeb]oOIB;ooPl:hoW@XFoo1Dblo?lXao3QPhno3n1koVPY_oOg`Eooc"
    A$ = A$ + "Wgno`O?noW?omoof^Oooc]lioCT^NoO?dVmoEXEPo31Cbm?4?IgoA4UNo71EmmO4F1hoO@FQoCP4IlO1BTao5HQ7oO08]lO2WPco"
    A$ = A$ + ":\B?o_P;2mO3cTdo?TcCo;1?Bm?51QeoILDGoc1CSmO8BIfoF<C@D10000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000@:XUHJD4dFo3HUNn_;6Ieo6HB?ok@?Hmo?ERko0E9^o;dUhn_@GNko3Ii]oCTUgn?;a9io9LcEo7QCbm_4<Ufo"
    A$ = A$ + "CLeNoo@F2n_3Fmgo[LWWo7eZ9ooCVJlo=EJao_DY5o_AO2lo<PdKo_@B]mo27ifo:T4Lo[0B_m_AMfko0f[do38_Ao_@BBkoVdfS"
    A$ = A$ + "o?P7^l?3hhdo0ZKbo_3RWn_>aRmo;1<ioGEaWo_5?2lo7XG\okPN\n?7iMjoJ\gZo?U\Bo?S>?no;6lcoGY`?ooQgJloE6]io3f^"
    A$ = A$ + "Jo_?[2moe4Jbo3CV1oO301fo?LDKooPB`m?4=Ago@ldMo3QDjm_7WehoFdcCo?P3Cl?1ALao4<Q6oKP6Ul?2Q0co9D2=o[`9glo2"
    A$ = A$ + "Z\co>hb?oo@<2m_4dHdoDL3BoK1>:eo4`dc<0000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000]86OAP2K"
    A$ = A$ + "AFNF=FjojL5JoK`9ml?2`\do=0THo34Won?ALnko4eY_o?TVjnO@ENko2Ii]oCBH2n?48Mfo:dR@oSP;7m?2_Pdo8hBAo[0<7m_5"
    A$ = A$ + "NMhoR0hZoccUkn_AK^ko4Ui^ogbMJn?2eDeo9`CGo[`?SmO20AfoP4GUo?DX5o_CXRlolakdoW6\9o?>=>ko:\R?oSdL2n_;O=go"
    A$ = A$ + "3D3Eo;3Y<o??_Fmo6AKfoCQJEnO1;UgobPGVoSG]9ooCbBmo=1Kdo_E]Co_AIVko6<3Co;@:ol?<ARkob\i`ok2UonO;Enko>dCE"
    A$ = A$ + "oGP8el_34Qfo>DTJoo0B^m?4<9go@hDMo3aCgmO<gIio7TA8o?P3Dl?1?Hao50a5oG`4Klo1J@bo8hQ:oW08^l_2S0co;DB<og0:"
    A$ = A$ + "el_3ZLco?XB=WoP9ad0000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000P<ZAhKi@HYoS2J4nO1U\co84SC"
    A$ = A$ + "oW@>Lmo26efo2mi`oC4X4o?AN6lo1Y9_okSTfno?DFkoP@FQoWP:olO2^Ldo90CBoW@<<mO2c0eo@<dGoGBJ<n?6]UioN<Y`oCRS"
    A$ = A$ + "kno<<:koELTHoS`:1m?2]Hdo80CCoW`=Fmo=@JkokTY_o_SVon?CTFlo`I[co_TWnno8PegoVPDEo;@6Vl_0Q4coTDX[ooBV2oO="
    A$ = A$ + "PRloC<EMoCP;;mo0W4doQ4gUoWSW4o?>L:lofT9`oWSTen?A\ego;LR=oG`8dlo6Q=hoNTFSo[0<4mo0JLbo<04GoOQKLnO6e9jo"
    A$ = A$ + ">D4Jok0B]mo3:1go@\DLoK1EimO<^Qho40Q5o?03Al?1=8ao4d`4oCP3ElO1ALao6@a6oOP5Ol?2I4bo8TQ8oWP6QlO2GdAX0000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000HU8X4LCK8B]DMRkoY07ToK0;7m?2eLeo94dIoS@A]m??K2lo"
    A$ = A$ + "11:aokSVnno>DRkok0i\o3bH3no2`Hdo7D2>oO@9il?2VXco8Pb>oS@:llO4oHeoiT8[okBNMno7[EioH87XoOANXno4T1io;lSG"
    A$ = A$ + "oS`9llo1ULcoDP4IoCcRano<<>kod4i]oGCThn?CP2loE=:`o?cORnO0@Lao2@A7o;05Nl_5IUgoUDH\oWbRgno5J1hoPDWWo3AA"
    A$ = A$ + "Sm?:nEjo]dX]oc2Seno:;BkoUXgWoW@8_lo0H@bo2DA8o7`4LlO0B`ao9lBBo?aG:n_616koR`iboSQLJno2dldo=h3Hok0AXmo3"
    A$ = A$ + "6]fo>H4Ko[QEkmo:Uigo5<Q6o;`2?lo0;4ao3`@4o?@3Bl?1=8ao4hP4oCP3ClO1?@ao5l05kOP5MH3000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000`H:^iC55XVokCS_no:nAjo74SCoS0>Lm?20Efo7hCIog2Q[no>Gfkoi4i]oCSR_n?6"
    A$ = A$ + "BAgoQ06PoGQ@Km_1N`bo6lA;oO`7^lo1Plbo70R;o3Q=<m?<kiio]DGVogRMHn_:aEioK`5Pok`AYm?4B]go>PDKoc`>Kmo5Raho"
    A$ = A$ + "O`FUoO2NNno:0Njo\8hZo3CQ]n_@BBkohPhZoO07Xl_0?Dao20a5oWP:llO7d5joHLUNo;A>=m_41YeoFDUMo;RLKnO9oYjoTdGZ"
    A$ = A$ + "o?BOXn_7\9io2<17o;@5Ol_0BXao3<Q6o[0>EmO4KEhoHd7[o32W9oo8T2moQPIao7QDfmO32=fo=0dHog@@Sm_33Mfo>D4JoCQC"
    A$ = A$ + "bmO;^UhoA`b=o702<l_0:h`o3\`3o?03?l?1<0ao4`04oC03@l?1=4aY00000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000gHFO6hDM9jLO\:loZdWYo3CQ^no2kTeo7<CDoO0=Dmo1b4eoJ`UOoC3ScnO;0FjoDlDLo7bH5n?;g]ioBXCDoG@6"
    A$ = A$ + "Tl_1IDbo6\Q9oKP6Ul_1IDbo>lR@o[2LBno9\ehoWT6SoG2J:nO67=fo4<a6oKP6Vl?2WXco9\R@o;QF2n?5[QioAdERoo`Cfm_4"
    A$ = A$ + "9YfoFdDKo7bGnm_:`9io>hB?o703@l_0=4ao5\a9oWP:ol?3aPdoH8fRoCBRjnO2[hcoKLVSooAMPn_7cmioM8WWoS1H6n_0@Hao"
    A$ = A$ + "2dP4o;`2?lO2^Ldo=PDKo;AIAno5jQjoIh7[o[QNYnO6d9Jj?PdJmg0@QmO4I=ho@<5Ook0AXmO305fo=44IokP@Um_8R5hoQ@eJ"
    A$ = A$ + "o[P7Wl_09``o2T03o?P2=lo09d`o3XP3RGP4IP10000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000@`6X`a0H<25000000000000000000000000000000000000000000`@^5HI"
    A$ = A$ + "Xa8WoOaC_mo:oIjo]4XZoO1D`m_1Wdco6LB?oK@9jlO2^<doT<GVo7ACcm?8RIhoVXVSoGRJ<nO4g`do4DQ7oG@5OlO1Elao5Da7"
    A$ = A$ + "oG@5Nl?3WPcoT@VQo;2H1n_8N1hoR0FPo7a<7m?1@Lao4416oC04FlO1D`aoFT4Jo7A@Qmo39afo?4ENoo0Dgm?32Mfo9@CDoS@:"
    A$ = A$ + "nlo1Q4co9Pb>o_0>Dm_2dldo;43Bo71Cam?5JIhoFl5Ro7P3Clo4>mfoIT6UoW1JCnO6XIio>l3Fo?P3Clo0?Lao7<b<oW0<9m?3"
    A$ = A$ + "o4fo>\DLo3ADiAO4G1X_ClERO=1G4n`2ch4U<PCEogP?Nmo4T1ioDTFUo71Emm_32Afo=hcGoc@?Nmo34EfoJDEMoS1CUmO3XLco"
    A$ = A$ + "6La7oKP5P8J1C\Q7000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000D`8hX`1n8VW48B=S;05K<>1KLBR5\1:6B06S<41H<285\a9G00000000000BTCCL8CInQ]3d`4j<03B\?2KDf_9"
    A$ = A$ + "e]ioQHfRoC07YlO1LXbo5XQ9ok@AUmo4H9hoNlUPo7bG1n?8MmgoQl5Po71=8m?1APao5<Q6oG`4Jl?1BTao48A6o[@8_l?8Lago"
    A$ = A$ + "NLeMog1Eem_7GQgo;@2<oC`3El?1?@ao4h`4oO`5QlO6<YFoIT4JSSQBX=k5:UFL?`CFGY0;3YZ1P0Co64b<o31C]mO6eAjo?ldL"
    A$ = A$ + "o?@4Hl_0:d`o9`R@og0@Rm_1Pdbo2`04oW@:il_5RihoF46Sog@>Am?1BTao8H2>oK`6Xl_1Plbn8L2?I[`;8=:3lTeF?T4KM000"
    A$ = A$ + "00000000000000aCaA03lXUP;DCDjc0>Fmo4NUhoGLWYoG1KKnO4@Qgo=\cFocP>Im?3kXeo<XSFok`?OY_3ohEO<HCC30000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000:lSI"
    A$ = A$ + "UX`A]m21G4R94HQ7gF08`8_1UPco5<R=oGP7\L_1W`Sl54B<3GP6W<H1L\RM@d2@=mACSAA4l<53KLeM_<2J;bU9bQiS;PR>nN08"
    A$ = A$ + "_\>5VihoBXUPok0<3m_7IUgoLD5Mog1EcmO7DAgo@8CAo?`3El?1@Lao40Q5oC04Gl?1@Hao8`Q9ogQEemo6@ifoKd4Ko[1D_mo1"
    A$ = A$ + "I<Bo4h`4ZC04FhL1@HQX9dA:W9a>Dm24lLe1000000000000000000000\@<7AS1KPRMM@h\`SALM>>1C\ae1X@3[G`6Xl?2TLco"
    A$ = A$ + "5XQ9o7@16lO0:h`o2\P3oO08\lO1H8bo2\`3oCP5Ml_1KHbo8Pb>nW`;5M42Zlc0000000000000000000000000000000000000"
    A$ = A$ + "00000003iHE?;<SC1_@=@]O4EagoJlW[o[AP`n_5[Mio?@TIo_@=AUo2f8E`>lCGm00000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000XP<51A2_049"
    A$ = A$ + "9\b>aH@7X4E1C\aY4<a61C@4I@=1Cdai50b;oOP;9eO2gHeo9<cC\O`:2AL1SHcT9\B?Pa0<4QC6g1:FQD9aZO1KIBL3bPD?:D2="
    A$ = A$ + "@9a>DI667A6PHHTH[JQAT]>4gldo48A6lO@5NH=2GlAd8La7AOP5MPl2NXb^Id4KKRaBX]W52aEGC\SDo\P8^TB2LL249la:2000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000P6caY^<T3EfTP:m@`1R4S378R<fI`7^HY1KLB>6dA:6EP6UXU1"
    A$ = A$ + "KHBN:P2>=:`2><_0;0Qi2l@5CG06Q<<2XTCG8hR@200000000000000000000000000000000000000000000000000000@3ode0"
    A$ = A$ + "<`CF_\P<;=g3;ifZJ0h[fkASmn?74BkoFHFT9G1I:Z1000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00009DCC1HP6V8B1C\1Q58a6XF@5M4L1G4Re5XQ9_G@7Z`_1ULSoJDWVoOBZFoO5PMXo5T194?04F0I1Fl1I6XQ9PM09h8L2o8fh"
    A$ = A$ + "9<TJoO@:o`]1PdbB000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000T0<81J2^@4=0000000000000000000000000000000000000000000000001X@3b:`2"
    A$ = A$ + "?X<1D`a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000P4"
    A$ = A$ + "NMH?JlW[To1U2oO8GBloLDH\XOaKK^e5]AI00000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000004ABUMP5NeG9G86QGbRZEW_:]ZmoHd6Vi>P3DLX0<8aX2dP4g:`2?0L1KPBj84CCoS@;7Eo1Q<CS6\a9?0000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000>XTK2[0=<5300000000000000000000000000000000000000000000000P0;l`H2\`3o;03Al4000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000004A1G7K4H\532U"
    A$ = A$ + "3o?9QjloQL9an_QO\ZJ6ci970000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000000000000000<QMU:`3b=:H9XURiRPCk9W5c9JQDdfV3:AICbF4WEiF"
    A$ = A$ + "FHgYDBaLQng4]a9JC`FVCM1OZBH79R;QHPgXFMQLLRD5\=9@BH6Sl01G1nS3BMW?<LdIlX@?LY32g8e?8DCD3Q`<>A42`TDA?PTI"
    A$ = A$ + "e5RSlNO<kJnoYHjdocAOX^93h`429T2>2L`8_8P1MTR05T191H06ULH1Eh1Q4@a7W@P4J<01BX1130a54<@3C@P0:l012T0348@2"
    A$ = A$ + "=@`0:h013X`34<P2>@`0:h013XP348@2<@@06T011@@140P03@000401000040000@000001000048`2?@P1O\216<B<4DP6VP05"
    A$ = A$ + "VAYf:DCCaHP8_<P1Qlb05T1934`1;<004H`00D`134@17<@06PP01H@224@2<`S0;l`o2\`387`19@@07X@01H@210@174003D@0"
    A$ = A$ + "04P01000040000@000001000040018@0080110017400000000000000000000000000000000@32YU2HD7XG62U3kO9T2moVHJd"
    A$ = A$ + "okQRh^N6biIG0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000T`jf6VaXCkK3UMIU8DePOSARiZM6=jkoI\h^o?aMXn?65FkoH0H\oGaO_nO5"
    A$ = A$ + "5JkoMPiajO2[Hk?9U6moQh9cogaU5o_6=jkoHDH]oGANZn_4[eio?46Tog@E1no289go9lCIoS0>I=O4E]GkV`Ybo32RhVo5UAhQ"
    A$ = A$ + "6\a8@000041000P40000B000081000`40000A0`0401000`40000D0000@1000@50000E0000D1000@50000F0000H1000P50000"
    A$ = A$ + "F0000H1000P50000F0000H1000P50000F0000H1000P50000F0000D1000@50000E0000D1000056l1;UXQPc6_2aLD@0000B000"
    A$ = A$ + "081000@40000A000041000040000@0000l0000`30<@1P8`2?0_0;l`n0H0231000`0000@30000<0000`0000030000;0000X00"
    A$ = A$ + "00P2000090000T0000@2000080@01L003@`11H@278`2?H`0@HA18LB=4X@<1A03khT0EDVRY52U2?_9TnloQDI`oKAKIBO5V=i_"
    A$ = A$ + "FTVUlE1I;6D4?]f100000TAJ=F`70N:?ZXY_7abQ[J0000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000`9XNMAC0WW24ACcQU4CY7[@h4MUn@B]9I4CeW^BDUOgn@AYU:3gLeU>44ICBaF7Vn5V5YiDdeQK?QF"
    A$ = A$ + "3f\4JAh`A<UNl2aDjIk3?EgY<D4JHZP=CMX1TPSL6l1<TAP6ZPe0ChaA1L03c8AC]M85J1h_:h2@I1@02P2000`:0000]0000d20"
    A$ = A$ + "00P;0000_0000l20000<0000`000043000@<0000a000043000P<0000b000083000`<0000c0000<3000`<0000c0000@30000="
    A$ = A$ + "0000d0000@30000=0000c0000@3000`<0000c0000<3000`<0000b0000832WHCDSh9cmW@;oh5000`;0000_0000h2000P;0000"
    A$ = A$ + "]0000d20000;0000\0000\2000P92T@35;`2?l_09d@b00@0X0000H2000P90000U0000@2000090000S000042000@80000P000"
    A$ = A$ + "0l1000@70000M0000\1000P60000I0000L1000P50000E0000<a0>8a3;D3B2m1Sj6N7=f[iR@ZdoCBZEo?8GFloBL5Pl7aCei;5"
    A$ = A$ + "LEXZGH6T8cAO\Z_<o^>iEm\o\nbPT>0000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000@5ZIY2E0VR?@AG6R@5W1910000000000@4HUG07D2="
    A$ = A$ + "6000041000`60000S0000P2000@;0000a0000@3000`=1D`1k0@01X3000@?0000o000044000P@000031000D4000PA00007100"
    A$ = A$ + "0P4000@B000091000X4000PB0000:1000`40000C0000<1000d4000@C0000=1000d4000@C0000>1000h4000PC0000>1000h40"
    A$ = A$ + "00PC0000>1000h4000PC0000>1000h4000PC0000=a`=:mg6nUjn;DcAl1000\40000C0000<1000\4000`B0000:1000X4000@B"
    A$ = A$ + "000091000@D08\`]2XP3^7`1:<;0140C000051000@4000`@00003100084000@@000001000l3000P?0000m0000`3000`>0000"
    A$ = A$ + "j0000P3000P=0000e000083000@<0000_0000`2000P:6\A9kP`9elc3=MVNOLY`QKbZGoO6f1joDD6TogaPcn?:Rj<m@@TG7MQF"
    A$ = A$ + "cQ0<=f:D1M;hm000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000aDb9`0AP120000@0000P1000P7"
    A$ = A$ + "0000S0000L2000`:0000^0000430000=0000e0000L3000@>0000k0000d3000`?000011000<40000A000051000H4000`A0000"
    A$ = A$ + "81000T4000PB0000;1000`40000C0000=1000h4000PC0000?1000l40000D0000@100045000@D0000A100085000PD0000B100"
    A$ = A$ + "0<5000`D0000C1000<5000`D3lP5Ua@AXA?1D`AJ0000D1000@50000E0000D1000D50000E0000D1000@50000E0000C1P02D60"
    A$ = A$ + "28PI0000>100085000PD0000A1000450000D0000?1000l4000PC0000>1000d40000C0000<1000\4000PB000091000P4000PA"
    A$ = A$ + "000041000<4000@@0000o0000d30000?0000i0000@c0><A>@`DIlYQNS2\3;MVOHlVTHVaI36G04H@3<dR>6\1Hk50000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000;HcC1P`:n<P0=<a11D02:0000d0000040000B0000D10"
    A$ = A$ + "00`50000H0000\1000@70000N0000l1000@80000S0000D2000P90000X0000T2000`:0000\0000d2000P;0000_000003000@<"
    A$ = A$ + "0000b0000<30000=0000e0000H3000`=0000g0000P3000@>0000j0000X3000P>0000k0000`30000?0000m0000d3000P?0000"
    A$ = A$ + "n0000lc1R8SLF8GXoG`6Y@70000B00002100084000P@00002100084000`@000031000<4000`@000031000<4000`@00002100"
    A$ = A$ + "084000P@00002100044000@@000011000040000@000001000l3000`?0000n0000h3000@?0000l0000\3000P>0000i0000P30"
    A$ = A$ + "00`=0000e0000@3000`<0000`0000d2000@:0<01S000082000`60000D0000d@4o8E1J`eM1000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000004P184004HP0080120@02<00140100@050000D0000P100007000"
    A$ = A$ + "0P000002000090000X0000`20000<0000d0000P30000?000001000040000A000081000`40000D0000D1000@50000F0000H10"
    A$ = A$ + "00`50000H0000P1000@60000J0000X1000P60000K0000`1000070000M0000d1000P70000N0000l1000080000P4QD`eh7HJlo"
    A$ = A$ + ";LCC:200083000`80000S0000<2000`80000S0000<2000090000T0000@2000090000T0000@2000090000S0000<2000`80000"
    A$ = A$ + "S0000<2000`80000R000082000P80000Q000042000@80000Q000002000080000O0000l1000P70000M0000`1000`60000K000"
    A$ = A$ + "0T1000060000G0000D1000`40000A0000h`0><Q2<lR?64a@H=059eE000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "1000040000@000002000080000P0000020000<0000`0000030000@000001000040000D0000@1000050000D0000@100005000"
    A$ = A$ + "0H0000P1000060000L0000`1000070000L000002000070000L003@022h058@06QP@1J@B2;H3CZ5AIAn_2`H4S0000K@06Q\`0"
    A$ = A$ + "@HQ21@P1:0000\0000P20000;0000X0000P20000:0000\0000`20000:0000X0000`20000;0000\0000P20000:0000X0000P2"
    A$ = A$ + "0000:0000X0000P2000090000X0000@2000090000T000002000080000P0000`1000080000L0000`108P070015H@05L@13l05"
    A$ = A$ + "5DP6R@@1LDb06lQ:3L09_802U0C0000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000`P?E5@32=fQGdWZoS@:0=X1NTR2;TcC1P`9g4P0=8A000001000"
    A$ = A$ + "040000@000001000040000@000001000040000@000001000040000@000001000040000@000001000040000@0000010000400"
    A$ = A$ + "00@0000010000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000XAKHRK7C6lo?L4JAn`BW9000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000005Weh1M`X^S?2YAo?6WYhW0000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\aP"
    A$ = A$ + "`RB7?f[nJ@X\ooAOV2;000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000F07W]0QIEbO4WEioAleR"
    A$ = A$ + "h100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000`4W=YIG<h\oSaQfnO4OahR0000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "000000000000000000000000000000000000000000000HQKNZZ7B6loOPYaoGAKLN:000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000FXFU>SAKGJ?8E><nF\FV4300000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000`2UTCA?lCG09QCcUB4<iF800000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    A$ = A$ + "00000000000000000000000000000000000000000000000000000000000000000000000000000000%%00"
    FOR i& = 1 TO LEN(A$) STEP 4: B$ = MID$(A$, i&, 4)
        IF INSTR(1, B$, "%") THEN
            FOR C% = 1 TO LEN(B$): F$ = MID$(B$, C%, 1)
                IF F$ <> "%" THEN C$ = C$ + F$
            NEXT: B$ = C$
            END IF: FOR t% = LEN(B$) TO 1 STEP -1
            B& = B& * 64 + ASC(MID$(B$, t%)) - 48
            NEXT: X$ = "": FOR t% = 1 TO LEN(B$) - 1
            X$ = X$ + CHR$(B& AND 255): B& = B& \ 256
    NEXT: btemp$ = btemp$ + X$: NEXT
    BASFILE$ = btemp$: btemp$ = ""

    DIM MemoryBlock AS _MEM, TempImage AS LONG
    TempImage = _NEWIMAGE(128, 128, 32)
    MemoryBlock = _MEMIMAGE(TempImage)

    memcpy MemoryBlock.OFFSET, _OFFSET(BASFILE$), LEN(BASFILE$)
    _MEMFREE MemoryBlock

    WASP_IMAGE& = TempImage
END FUNCTION
