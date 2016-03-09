'vWATCH64 - A debug/variable watch system for QB64 programs
'Fellippe Heitor, 2015/2016 - fellippeheitor@gmail.com - @fellippeheitor

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

'Custom type library for Steve's File Selection Utility:
DECLARE CUSTOMTYPE LIBRARY "direntry"
    FUNCTION FILE_load_dir& ALIAS load_dir (s AS STRING)
    FUNCTION FILE_has_next_entry& ALIAS has_next_entry ()
    SUB FILE_close_dir ALIAS close_dir ()
    SUB FILE_get_next_entry ALIAS get_next_entry (s AS STRING, flags AS LONG, file_size AS LONG)
    SUB FILE_get_current_dir ALIAS get_current_dir (s AS STRING)
    FUNCTION FILE_current_dir_length& ALIAS current_dir_length ()
END DECLARE

'Constants: -------------------------------------------------------------------
CONST ID = "vWATCH64"
CONST VERSION = ".962b"

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
DIM SHARED DEFAULTDATATYPE(65 TO 90) AS STRING * 20
DIM SHARED EXCHANGEDATASIZE$4
DIM SHARED EXCHANGEDATA AS STRING
DIM SHARED FILE AS INTEGER
DIM SHARED FILENAME$
DIM SHARED FILEERRORRAISED AS _BIT
DIM SHARED CONVERSIONERRORRAISED AS _BIT
DIM SHARED PAGE_HEIGHT AS LONG
DIM SHARED INTERNALKEYWORDS AS INTEGER
DIM SHARED LIST_AREA AS INTEGER
DIM SHARED LINE_TRAIL AS INTEGER
DIM SHARED LONGESTLINE AS LONG
DIM SHARED MAINSCREEN AS LONG
DIM SHARED MENU%
DIM SHARED MESSAGEBOX_RESULT AS INTEGER
DIM SHARED NEWFILENAME$
DIM SHARED SB_TRACK AS INTEGER
DIM SHARED SCREEN_WIDTH AS INTEGER
DIM SHARED SCREEN_HEIGHT AS INTEGER
DIM SHARED SET_OPTIONBASE AS INTEGER
DIM SHARED SOURCEFILE AS STRING
DIM SHARED CHECKINGOFF_LINES AS STRING
DIM SHARED TITLESTRING AS STRING
DIM SHARED TOTALBREAKPOINTS AS LONG
DIM SHARED TOTALVARIABLES AS LONG
DIM SHARED TTFONT AS LONG
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

'Switches:
DIM SHARED DONTCOMPILE AS _BIT
DIM SHARED FIRSTPROCESSING AS _BIT
DIM SHARED NO_TTFONT AS _BIT
DIM SHARED STEPMODE AS _BIT
DIM SHARED SKIPARRAYS AS _BIT
DIM SHARED TIMED_OUT AS _BIT
DIM SHARED USERQUIT AS _BIT
DIM SHARED CLOSE_SESSION AS _BIT
DIM SHARED DEBUGGEE_CLOSED AS _BIT
DIM SHARED TRACE AS _BIT
DIM SHARED VARIABLE_HIGHLIGHT AS _BIT

'Dynamic arrays:
REDIM SHARED QB64KEYWORDS(0) AS STRING
REDIM SHARED SOURCECODE(0) AS STRING
REDIM SHARED SOURCECODE_COLORIZED(0) AS _BYTE
REDIM SHARED SUBFUNC(0) AS SUBFUNC_TYPE
REDIM SHARED SUBFUNC_ENDLINE(0) AS LONG
REDIM SHARED VARIABLES(0) AS VARIABLESTYPE
REDIM SHARED VARIABLE_DATA(0) AS VARIABLEVALUETYPE
REDIM SHARED WATCHPOINT(0) AS WATCHPOINTTYPE

DIM i AS INTEGER

'Variables initialization: ----------------------------------------------------
FOR i = 65 TO 90
    DEFAULTDATATYPE(i) = "SINGLE"
NEXT i
SET_OPTIONBASE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0
NO_TTFONT = 0
FIRSTPROCESSING = -1
VARIABLE_HIGHLIGHT = -1
SCREEN_WIDTH = DEFAULT_WIDTH
SCREEN_HEIGHT = DEFAULT_HEIGHT
LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR
SB_TRACK = LIST_AREA - 48

RESTORE_LIBRARY
READ_KEYWORDS

'Screen setup: ----------------------------------------------------------------
MAINSCREEN = _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
SCREEN MAINSCREEN
DO: _LIMIT 30: LOOP UNTIL _SCREENEXISTS
TITLESTRING = "vWATCH64 - v" + VERSION
_TITLE TITLESTRING

'Parse the command line: ------------------------------------------------------
'Did the user drag a .BAS file onto this program or enter parameters?
'Syntax: VWATCH64 [source filename.bas] [-options] [-target <newfilename>]
'If no parameters are passed, vWATCH64 starts in MONITOR MODE
IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT > 1 THEN
        'Set flags based on command line arguments:
        FOR i = 1 TO _COMMANDCOUNT
            SELECT CASE LCASE$(COMMAND$(i))
                CASE "-dontcompile", "-d": DONTCOMPILE = -1
                CASE "-noarrays", "-n": SKIPARRAYS = -1
                CASE "-target", "-t": IF i < _COMMANDCOUNT THEN NEWFILENAME$ = COMMAND$(i + 1): i = i + 1
                CASE "-font16", "-f16": NO_TTFONT = -1
                CASE ELSE
                    'Any other arguments are ignored.
            END SELECT
        NEXT i
    ELSEIF _COMMANDCOUNT = 1 THEN
        IF LCASE$(COMMAND$(1)) = "-font16" OR LCASE$(COMMAND$(1)) = "-f16" THEN NO_TTFONT = -1
    END IF
END IF

$IF WIN THEN
    'Under Windows, if Lucida Console font is found, it is used;
    'Otherwise we stick to _FONT 16 (default):
    IF NO_TTFONT = 0 THEN TTFONT = _LOADFONT("C:\windows\fonts\lucon.ttf", 14, "MONOSPACE")
    IF TTFONT > 0 AND NO_TTFONT = 0 THEN _FONT TTFONT
    PATHSEP$ = "\"
$ELSE
    PATHSEP$ = "/"
$END IF

IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT = 1 AND NO_TTFONT = 0 THEN
        IF _FILEEXISTS(COMMAND$(1)) THEN FILENAME$ = COMMAND$(1): PROCESSFILE ELSE MESSAGEBOX_RESULT = MESSAGEBOX(ID, "File not found.", MKI$(OK_ONLY), 1, 0)
        NEWFILENAME$ = "": FIRSTPROCESSING = 0
    ELSEIF _COMMANDCOUNT > 1 THEN
        IF _FILEEXISTS(COMMAND$(1)) THEN FILENAME$ = COMMAND$(1): PROCESSFILE ELSE MESSAGEBOX_RESULT = MESSAGEBOX(ID, "File not found.", MKI$(OK_ONLY), 1, 0)
        NEWFILENAME$ = "": FIRSTPROCESSING = 0
    END IF
END IF

GOTO MainLoop
OpenFileMenu:
IF SCREEN_WIDTH < DEFAULT_WIDTH OR SCREEN_HEIGHT < DEFAULT_HEIGHT THEN CHECK_RESIZE DEFAULT_WIDTH, DEFAULT_HEIGHT
_RESIZE OFF
CLS , _RGB32(255, 255, 255)
_PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
t$ = "Fetching file list..."
_PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - (_FONTHEIGHT / 2) + _FONTHEIGHT), t$
_DISPLAY
FILENAME$ = SelectFile$("*.BAS;*.*", _WIDTH(MAINSCREEN) / 2 - 320, _HEIGHT(MAINSCREEN) / 2 - 240)
_RESIZE ON

'Reset flags:
FOR i = 65 TO 90
    DEFAULTDATATYPE(i) = "SINGLE"
NEXT i
SET_OPTIONBASE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0
FIRSTPROCESSING = 0

IF _FILEEXISTS(FILENAME$) THEN PROCESSFILE
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
DATA WRITE,XOR,_CEIL,**END**

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
    STATIC RunToThisLine AS LONG

    TotalButtons = 7
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    TOTALBREAKPOINTS = 0
    BREAKPOINT.ACTION = 0 'Start paused; execution starts with F5 or F8.
    BREAKPOINT.LINENUMBER = 0
    PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS , _RGB32(255, 255, 255)

    Filter$ = ""
    IF SearchIn = 0 THEN SearchIn = CODE
    SB_ThumbY = 0
    grabbedY = -1
    ListEnd_Label = "(end of source file)"
    ShowContextualMenu = 0
    ShowTempMessage = 0
    STEPMODE = -1
    TRACE = -1
    _KEYCLEAR
    TIMED_OUT = 0
    CLOSE_SESSION = 0
    DEBUGGEE_CLOSED = 0

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
                IF INSTR(UCASE$(VARIABLES(i).SCOPE), GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)) = 0 AND TRIM$(VARIABLES(i).SCOPE) <> "SHARED" THEN
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
                IF CLIENT.LINENUMBER = RunToThisLine THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    RunToThisLine = 0
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
                VARIABLE_VIEW
            END IF
        END IF

        GOSUB UpdateList

        IF _EXIT THEN USERQUIT = -1
        SEND_PING
    LOOP UNTIL HEADER.CONNECTED = 0 OR USERQUIT OR TIMED_OUT OR CLOSE_SESSION OR DEBUGGEE_CLOSED

    EndMessage:
    IF CLOSE_SESSION OR USERQUIT THEN
        HEADER.CONNECTED = 0
        PUT #FILE, HEADERBLOCK, HEADER
        CLOSE #FILE
        ON ERROR GOTO FileError
        DO WHILE _FILEEXISTS(_CWD$ + PATHSEP$ + "vwatch64.dat")
            KILL _CWD$ + PATHSEP$ + "vwatch64.dat"
        LOOP
        ON ERROR GOTO 0
        EXIT SUB
    END IF

    IF USERQUIT THEN EXIT SUB

    IF HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED THEN
        EndMessage$ = "Connection closed by client."
    ELSEIF TIMED_OUT THEN
        EndMessage$ = "Connection timed out."
    END IF

    IF HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED OR TIMED_OUT THEN
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

    IF my < SCREEN_TOPBAR AND ShowContextualMenu THEN ShowContextualMenu = 0

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
            IF ShowContextualMenu THEN
                ShowContextualMenu = 0
            ELSEIF (SearchIn = CODE OR SearchIn = LINENUMBERS) AND LEN(Filter$) > 0 THEN
                Filter$ = ""
            ELSEIF SearchIn = SETNEXT THEN
                SearchIn = PrevSearchIn
                Filter$ = PrevFilter$
            ELSE
                CLOSE_SESSION = -1
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 18432 'Up
            IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - ((_HEIGHT - 50) * SB_Ratio)
            TRACE = 0
        CASE 20480 'Down
            IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + ((_HEIGHT - 50) * SB_Ratio)
            TRACE = 0
        CASE 16128 'F5
            RunButton_Click:
            IF WATCHPOINTBREAK > 0 THEN
                IF ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 1 THEN
                    Message$ = "Execution was halted on a watchpoint (" + TRIM$(VARIABLES(WATCHPOINTBREAK).NAME) + TRIM$(WATCHPOINT(WATCHPOINTBREAK).EXPRESSION) + ")" + CHR$(LF)
                    Message$ = Message$ + "Clear it before resuming?"
                    MESSAGEBOX_RESULT = MESSAGEBOX("Run/Resume", Message$, MKI$(YN_QUESTION), 1, -1)
                    IF MESSAGEBOX_RESULT = MB_YES THEN
                        ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 0
                        WATCHPOINT(WATCHPOINTBREAK).EXPRESSION = ""
                        PUT #FILE, WATCHPOINTLISTBLOCK, WATCHPOINTLIST
                        PUT #FILE, WATCHPOINTEXPBLOCK, WATCHPOINT()
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
                VARIABLE_VIEW
            ELSE
                Message$ = "There are no variables in your watch list."
                MESSAGEBOX_RESULT = MESSAGEBOX("No variables", Message$, MKI$(OK_ONLY), 1, -1)
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16896 'F8
            StepButton_Click:
            IF shiftDown THEN
                IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                STEPMODE = -1
                TRACE = -1
                BREAKPOINT.ACTION = SKIPSUB
                PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            ELSE
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
                IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
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
            ELSE
                FOR setAll = 1 TO LEN(FilteredList$) / 4
                    which_Line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_Line) = 0 AND LEN(STRIPCOMMENTS$(GETLINE$(which_Line))) THEN
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
                TOTALBREAKPOINTS = 0
                BREAKPOINTLIST = STRING$(CLIENT.TOTALSOURCELINES, 0)
            ELSE
                FOR setAll = 1 TO LEN(FilteredList$) / 4
                    which_Line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_Line) = 1 THEN
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

        IF ShowContextualMenu AND (ContextualMenu.FilteredList$ <> FilteredList$) THEN ShowContextualMenu = 0

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

        IF ShowContextualMenu AND (y <> ContextualMenuYRef) THEN
            ShowContextualMenu = 0
        END IF

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
                    IF SOURCECODE_COLORIZED(i) = 0 THEN ADDCOLORCODE i
                    v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), IIFSTR$(ASC(BREAKPOINTLIST, i) = 2, CHR$(9), " ")) + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
                    PRINT_COLORIZED 5, printY, v$, i
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
                IF SOURCECODE_COLORIZED(i) = 0 THEN ADDCOLORCODE i
                v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), IIFSTR$(ASC(BREAKPOINTLIST, i) = 2, CHR$(9), " ")) + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
                PRINT_COLORIZED 5, printY, v$, i
                COLOR _RGB32(0, 0, 0)
            NEXT i
        END IF

        IF ShowContextualMenu THEN GOSUB DetectClick

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
    Buttons(B).ID = 4: Buttons(B).CAPTION = IIFSTR$(STEPMODE, IIFSTR$(shiftDown = -1, "<F8=Step " + IIFSTR$(TRIM$(CLIENT_CURRENTMODULE) = "MAIN MODULE", "Over", "Out") + ">", "<F8=Step>"), "<F8=Pause>"): B = B + 1
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

    IF NOT ShowContextualMenu THEN
        GOSUB CheckButtons
    ELSE
        _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$
    END IF

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

    'Show contextual menu
    IF ShowContextualMenu THEN
        DrawContextualMenu:
        LINE (ContextualMenu.X, ContextualMenu.Y)-STEP(ContextualMenu.W - 1, ContextualMenu.H - 1), _RGB32(200, 200, 200), BF
        LINE (ContextualMenu.X, ContextualMenu.Y)-STEP(ContextualMenu.W - 1, ContextualMenu.H - 1), _RGB32(0, 0, 0), B
        IF MouseHeld THEN RETURN
        GOSUB CheckButtons
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
        IF i = RunToThisLine THEN BreakpointColor~& = _RGBA32(255, 255, 0, 200): COLOR _RGB32(0, 0, 0)
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT), BreakpointColor~&, BF
    END IF
    '...if "skip this line" is set,...
    IF ASC(BREAKPOINTLIST, i) = 2 THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(255, 255, 0, 200), BF
    END IF
    '...and if it was right-clicked before.
    IF (ShowContextualMenu AND ContextualMenu.printY = printY) THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(255, 255, 0, 200), BF
    END IF
    IF (ShowTempMessage AND TempMessage.printY = printY) THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(255, 255, 0, 255 - (170 * FadeStep#)), BF
    END IF
    RETURN

    DetectClick:
    'Hover:
    IF ShowContextualMenu = 0 AND STEPMODE THEN LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 50), BF

    'Select/Clear the item if a mouse click was detected.
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
            IF ShowContextualMenu THEN
                PCOPY 1, 0
                GOSUB DrawContextualMenu
                GOSUB MenuHoverHighlight
                _DISPLAY
            END IF
            SEND_PING
            mb = _MOUSEINPUT
            my = _MOUSEY
            mx = _MOUSEX
        WEND
        MouseHeld = 0
        mb = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click: RETURN

        IF ShowContextualMenu THEN
            IF (my > ContextualMenu.Y) AND (my < ContextualMenu.Y + ContextualMenu.H) AND (mx > ContextualMenu.X) AND (mx < ContextualMenu.X + ContextualMenu.W) THEN
                'Click on contextual menu
                IF (my >= ContextualMenu.Y + 4) AND (my <= ContextualMenu.Y + 4 + _FONTHEIGHT) THEN
                    Clicked = -1
                    DesiredLine = ContextualMenuLineRef
                    ShowContextualMenu = 0
                    GOSUB SetNext_Click
                ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) THEN
                    'Toggle breakpoint:
                    IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1 THEN
                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 0
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                    ELSE
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
                    ShowContextualMenu = 0
                ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 3) THEN
                    'Run to this line
                    IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1 THEN
                        RunToThisLine = 0
                    ELSE
                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1
                        RunToThisLine = ContextualMenuLineRef
                        FOR MultiLineToggle = ContextualMenuLineRef + 1 TO CLIENT.TOTALSOURCELINES
                            IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                                ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, ContextualMenuLineRef)
                            ELSE
                                EXIT FOR
                            END IF
                        NEXT MultiLineToggle
                    END IF
                    Clicked = -1
                    GOSUB RunButton_Click
                    ShowContextualMenu = 0
                ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT * 3) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 4) THEN
                    'Skip this line:
                    IF ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2 THEN
                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 0
                    ELSE
                        ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2
                    END IF
                    FOR MultiLineToggle = ContextualMenuLineRef + 1 TO CLIENT.TOTALSOURCELINES
                        IF RIGHT$(TRIM$(GETLINE$(MultiLineToggle - 1)), 1) = "_" THEN
                            ASC(BREAKPOINTLIST, MultiLineToggle) = ASC(BREAKPOINTLIST, ContextualMenuLineRef)
                        ELSE
                            EXIT FOR
                        END IF
                    NEXT MultiLineToggle
                    ShowContextualMenu = 0
                END IF
            ELSE
                'Click outside contextual menu
                ShowContextualMenu = 0
            END IF
        ELSE
            temp.SourceLine$ = UCASE$(STRIPCOMMENTS$(TRIM$(SourceLine)))
            e1$ = GETELEMENT$(temp.SourceLine$, 1)
            e2$ = GETELEMENT$(temp.SourceLine$, 2)
            PrevDesiredSourceLine$ = " "
            IF i > 1 THEN PrevDesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(i - 1)))
            SELECT CASE e1$
                CASE "DIM", "DATA", "CASE", "TYPE", "REDIM", "CONST", "STATIC", "DEFINT", "DEFLNG", "DEFSTR", "DEFSNG", "DEFDBL", "DECLARE", "_DEFINE", "SUB", "FUNCTION"
                    GOSUB TurnOnNonexecutableMessage
                CASE "END"
                    IF e2$ = "DECLARE" THEN
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
                            DoubleClick = 0
                            ASC(BREAKPOINTLIST, i) = 2
                        ELSE
                            'Toggle breakpoint/skip this line:
                            IF ASC(BREAKPOINTLIST, i) = 1 THEN
                                ASC(BREAKPOINTLIST, i) = 0
                                TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                            ELSEIF ASC(BREAKPOINTLIST, i) = 2 THEN
                                ASC(BREAKPOINTLIST, i) = 0
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
    END IF

    'Turn on contextual options if right mouse click and while in step mode.
    IF mb2 = -1 AND STEPMODE = -1 THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(2): _LIMIT 500: SEND_PING: mb2 = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb2 = 0
        temp.SourceLine$ = UCASE$(STRIPCOMMENTS$(TRIM$(SourceLine)))
        e1$ = GETELEMENT$(temp.SourceLine$, 1)
        e2$ = GETELEMENT$(temp.SourceLine$, 2)
        PrevDesiredSourceLine$ = " "
        IF i > 1 THEN PrevDesiredSourceLine$ = TRIM$(STRIPCOMMENTS$(GETLINE$(i - 1)))

        SELECT CASE e1$
            CASE "DIM", "DATA", "CASE", "TYPE", "REDIM", "CONST", "STATIC", "DEFINT", "DEFLNG", "DEFSTR", "DEFSNG", "DEFDBL", "DECLARE", "_DEFINE", "SUB", "FUNCTION"
                GOSUB TurnOnNonexecutableMessage
            CASE "END"
                IF e2$ = "DECLARE" THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSE
                    GOTO EndAllowed2
                END IF
            CASE ELSE
                EndAllowed2:
                IF LEN(temp.SourceLine$) = 0 THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF LEFT$(temp.SourceLine$, 1) = "$" OR LEFT$(temp.SourceLine$, 1) = CHR$(1) THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF RIGHT$(PrevDesiredSourceLine$, 1) = "_" THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSEIF ASC(CHECKINGOFF_LINES, i) THEN
                    GOSUB TurnOnNonexecutableMessage
                ELSE
                    IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
                        'Set contextual menu coordinates relative to this item
                        ShowContextualMenu = -1
                        ContextualMenuYRef = y
                        ContextualMenuLineRef = i
                        ContextualMenu.printY = printY
                        ContextualMenu.FilteredList$ = FilteredList$
                        ContextualMenu.W = _PRINTWIDTH(" Set next statement ") + 6
                        ContextualMenu.H = _FONTHEIGHT * 4.5
                        ContextualMenu.X = mx: IF ContextualMenu.X + ContextualMenu.W > _WIDTH THEN ContextualMenu.X = _WIDTH - ContextualMenu.W
                        ContextualMenu.Y = my: IF ContextualMenu.Y + ContextualMenu.H > _HEIGHT THEN ContextualMenu.Y = _HEIGHT - ContextualMenu.H
                    END IF
                END IF
        END SELECT
    END IF
    RETURN

    TurnOnNonexecutableMessage:
    IF NOT ShowContextualMenu THEN
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
    ELSE
        ShowContextualMenu = 0
    END IF
    RETURN

    CheckButtons:
    IF ShowContextualMenu THEN
        MenuHoverHighlight:
        IF (mx >= ContextualMenu.X) AND (mx <= ContextualMenu.X + ContextualMenu.W) THEN
            IF (my >= ContextualMenu.Y + 4) AND (my <= ContextualMenu.Y + 4 + _FONTHEIGHT) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4 + _FONTHEIGHT)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 3) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4 + _FONTHEIGHT * 2)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT * 3) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 4) THEN
                LINE (ContextualMenu.X + 2, ContextualMenu.Y + 4 + _FONTHEIGHT * 3)-STEP(ContextualMenu.W - 5, _FONTHEIGHT - 1), _RGB32(0, 178, 179), BF
            END IF
        END IF

        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4), " Set next statement "
        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4 + _FONTHEIGHT), IIFSTR$(ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 1, " Clear breakpoint   ", " Set breakpoint     ")
        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4 + _FONTHEIGHT * 2), " Run to this line   "
        _PRINTSTRING (ContextualMenu.X, ContextualMenu.Y + 4 + _FONTHEIGHT * 3), IIFSTR$(ASC(BREAKPOINTLIST, ContextualMenuLineRef) = 2, " Reactivate line    ", " Skip this line     ")
        IF MouseHeld = -1 THEN RETURN
    ELSE
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
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
FUNCTION GETELEMENT$ (SourceLine$, Element)
    SEP$ = " =<>+-/\^:;,*()" + CHR$(1) + CHR$(3) + CHR$(4) + CHR$(5)
    i$ = SourceLine$
    Position = 0
    InQuote = 0
    ThisElement = 0
    DO
        Position = Position + 1
        a$ = UCASE$(i$)
        CommentStart = LEN(STRIPCOMMENTS$(a$))
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
        ThisElement = ThisElement + 1
        IF ThisElement = Element THEN GETELEMENT$ = Element$
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
                ThisKW$ = UCASE$(TRIM$(SUBFUNC(j).NAME))
                IF a$ = ThisKW$ THEN
                    IF Start < CommentStart AND InQuote = 0 THEN
                        ColorCode$ = CHR$(5)
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
    DIM CommentStart AS LONG

    SEP$ = " =<>+-/\^:;,*()"

    CommentStart = LEN(STRIPCOMMENTS$(v$))

    DO
        Position = Position + 1
        'Lines with breakpoints, skip flags or in $CHECKING/VWATCH:OFF blocks are printed in white/gray
        IF ASC(BREAKPOINTLIST, SourceLineNumber) = 1 THEN COLOR _RGB32(255, 255, 255): GOTO ColorSet
        IF ASC(BREAKPOINTLIST, SourceLineNumber) = 2 THEN COLOR _RGB32(180, 180, 180): GOTO ColorSet
        IF ASC(CHECKINGOFF_LINES, SourceLineNumber) AND Position >= LINE_TRAIL THEN COLOR _RGB32(170, 170, 170): GOTO ColorSet

        IF ASC(v$, Position) = 34 OR InQuote THEN
            'Text in "quotation marks"
            IF ASC(v$, Position) = 34 THEN InQuote = NOT InQuote
            COLOR _RGB32(255, 165, 0)
            GOTO ColorSet
        END IF
        IF INSTR(SEP$, MID$(v$, Position, 1)) > 0 AND (MetaCommand > 0 OR KeyWord > 0 OR IsNumber > 0 OR IsSubFunc > 0) AND NOT InQuote THEN
            IF KeyWord > 0 THEN KeyWord = 0
            IF IsSubFunc > 0 THEN IsSubFunc = 0
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
    IF (Char = 1 OR Char = 3 OR Char = 4 OR Char = 5) AND NOT InQuote THEN
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
    longestVarName = 1
    longestScopeSpec = 1
    ShowContextualMenu = 0

    FOR i = 1 TO CLIENT.TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > longestVarName THEN longestVarName = LEN(TRIM$(VARIABLES(i).NAME))
        IF LEN(TRIM$(VARIABLES(i).SCOPE)) > longestScopeSpec THEN longestScopeSpec = LEN(TRIM$(VARIABLES(i).SCOPE))
    NEXT i

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
                IF INSTR(UCASE$(VARIABLES(i).SCOPE), GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)) = 0 AND TRIM$(VARIABLES(i).SCOPE) <> "SHARED" THEN
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
    LOOP UNTIL USERQUIT OR CLOSE_SESSION OR SWITCH_VIEW OR TIMED_OUT OR HEADER.CONNECTED = 0 OR DEBUGGEE_CLOSED

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

    IF my < SCREEN_TOPBAR AND (ShowContextualMenu OR ShowPopupWatchpoint) THEN ShowContextualMenu = 0: ShowPopupWatchpoint = 0

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
            IF ShowContextualMenu THEN
                ShowContextualMenu = 0
            ELSEIF LEN(Filter$) THEN
                Filter$ = ""
            ELSE
                CLOSE_SESSION = -1
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 18432 'Up
            IF PAGE_HEIGHT > LIST_AREA THEN
                IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - ((_HEIGHT - 50) * SB_Ratio)
            END IF
        CASE 20480 'Down
            IF PAGE_HEIGHT > LIST_AREA THEN
                IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + ((_HEIGHT - 50) * SB_Ratio)
            END IF
        CASE 16128 'F5
            RunButton_Click:
            IF WATCHPOINTBREAK > 0 THEN
                IF ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 1 THEN
                    Message$ = "Execution was halted on a watchpoint (" + TRIM$(VARIABLES(WATCHPOINTBREAK).NAME) + TRIM$(WATCHPOINT(WATCHPOINTBREAK).EXPRESSION) + ")" + CHR$(LF)
                    Message$ = Message$ + "Clear it before resuming?"
                    MESSAGEBOX_RESULT = MESSAGEBOX("Run/Resume", Message$, MKI$(YN_QUESTION), 1, -1)
                    IF MESSAGEBOX_RESULT = MB_YES THEN
                        ASC(WATCHPOINTLIST, WATCHPOINTBREAK) = 0
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
            IF shiftDown THEN
                IF STEPMODE = 0 THEN SetPause# = TIMER: ShowPauseIcon = -1: ShowRunIcon = 0
                STEPMODE = -1
                TRACE = -1
                BREAKPOINT.ACTION = SKIPSUB
                PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            ELSE
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
                IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
                    ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 0
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
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
            TOTALBREAKPOINTS = 0
            BREAKPOINTLIST = STRING$(CLIENT.TOTALSOURCELINES, 0)
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

    IF ShowContextualMenu AND (ContextualMenu.FilteredList$ <> FilteredList$) THEN ShowContextualMenu = 0

    CHECK_SCREEN_LIMITS y

    IF ShowContextualMenu AND (y <> ContextualMenuYRef) THEN
        ShowContextualMenu = 0
    END IF

    'Place a light gray rectangle under the column that can currently be filtered
    SELECT CASE SearchIn
        CASE DATATYPES
            columnHighlightX = _PRINTWIDTH(SPACE$(longestScopeSpec + 1))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(21)) + _PRINTWIDTH(SPACE$(longestScopeSpec + 1))
            columnHighlightW = _PRINTWIDTH(SPACE$(longestVarName)) + 8
        CASE VALUES
            columnHighlightX = _PRINTWIDTH(SPACE$(longestVarName)) + _PRINTWIDTH(SPACE$(20)) + _PRINTWIDTH(SPACE$(longestScopeSpec + 1)) + 16
            columnHighlightW = _WIDTH
        CASE SCOPE
            columnHighlightX = 0
            columnHighlightW = _PRINTWIDTH(SPACE$(longestScopeSpec)) + 8
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
            v$ = LEFT$(VARIABLES(i).SCOPE, longestScopeSpec) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + TRIM$(VARIABLE_DATA(i).VALUE)
            printY = ((3 + ii) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO CLIENT.TOTALVARIABLES
            v$ = LEFT$(VARIABLES(i).SCOPE, longestScopeSpec) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + TRIM$(VARIABLE_DATA(i).VALUE)
            printY = ((3 + i) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT i
    END IF
    COLOR _RGB32(0, 0, 0)
    IF ShowContextualMenu THEN GOSUB DetectClick

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
        LINE (tl.x, _FONTHEIGHT + 3)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(200, 0, 0, 200), BF
    END IF
    IF SOURCECODE_COLORIZED(CLIENT.LINENUMBER) = 0 THEN ADDCOLORCODE CLIENT.LINENUMBER
    TopLine$ = SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(CLIENT.LINENUMBER)))) + TRIM$(STR$(CLIENT.LINENUMBER)) + " " + CHR$(16) + " " + SourceLine
    PRINT_COLORIZED tl.x, _FONTHEIGHT + 3, TopLine$, CLIENT.LINENUMBER
    COLOR _RGB32(0, 0, 0)
    TopLine$ = "Filter: " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), TopLine$

    'Top buttons:
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F5=Run>": b = b + 1
    Buttons(b).ID = 2: Buttons(b).CAPTION = "<F6=Source>": b = b + 1
    Buttons(b).ID = 3: Buttons(b).CAPTION = IIFSTR$(STEPMODE, IIFSTR$(shiftDown = -1, "<F8=Step " + IIFSTR$(TRIM$(CLIENT_CURRENTMODULE) = "MAIN MODULE", "Over", "Out") + ">", "<F8=Step>"), "<F8=Pause>"): b = b + 1
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

    IF NOT ShowContextualMenu THEN
        GOSUB CheckButtons
    ELSE
        _PRINTSTRING (5 + _PRINTWIDTH(ModeTitle$), 3), ButtonLine$
    END IF
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

    'Show contextual menu
    IF ShowContextualMenu THEN
        DrawContextualMenu:
        LINE (ContextualMenu.X, ContextualMenu.Y)-STEP(ContextualMenu.W - 1, ContextualMenu.H - 1), _RGB32(200, 200, 200), BF
        LINE (ContextualMenu.X, ContextualMenu.Y)-STEP(ContextualMenu.W - 1, ContextualMenu.H - 1), _RGB32(0, 0, 0), B
        IF MouseHeld THEN RETURN
        GOSUB CheckButtons
    END IF
    _DISPLAY
    RETURN

    ColorizeSelection:
    'Indicate that this variable is used in the current source line
    IF VARIABLE_HIGHLIGHT THEN
        vs$ = TRIM$(VARIABLES(i).NAME)
        IF INSTR(vs$, "(") THEN vs$ = LEFT$(vs$, INSTR(vs$, "(") - 1)
        Element = 0
        DO
            Element = Element + 1
            a$ = GETELEMENT$(SourceLine, Element)
            IF a$ = "" THEN EXIT DO
            IF UCASE$(a$) = UCASE$(vs$) AND (INSTR(UCASE$(VARIABLES(i).SCOPE), GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)) > 0 OR TRIM$(VARIABLES(i).SCOPE) = "SHARED") THEN
                LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 0, 100), BF
            END IF
        LOOP
    END IF
    'or that it was right-clicked:
    IF (ShowContextualMenu AND ContextualMenu.printY = printY) THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(102, 255, 102, 200), BF
    END IF
    COLOR _RGB(0, 0, 0)

    'or that it has a watchpoint set
    IF ASC(WATCHPOINTLIST, i) = 1 THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(255, 0, 0, 200), BF
        COLOR _RGB(255, 255, 255)
    END IF
    RETURN

    DetectClick:
    'Hover/Watchpoint popup:
    IF ShowContextualMenu = 0 AND STEPMODE THEN
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
            IF ShowContextualMenu THEN
                PCOPY 1, 0
                GOSUB DrawContextualMenu
                GOSUB MenuHoverHighlight
                _DISPLAY
            END IF
            SEND_PING
            mb = _MOUSEINPUT
            my = _MOUSEY
            mx = _MOUSEX
        WEND
        MouseHeld = 0
        mb = 0

        IF STEPMODE = 0 THEN Clicked = -1: GOSUB StepButton_Click: RETURN

        IF ShowContextualMenu AND (my > ContextualMenu.Y) AND (my < ContextualMenu.Y + ContextualMenu.H) AND (mx > ContextualMenu.X) AND (mx < ContextualMenu.X + ContextualMenu.W) THEN
            'Click on contextual menu
            ShowContextualMenu = 0
            IF (my >= ContextualMenu.Y + 4) AND (my <= ContextualMenu.Y + 4 + _FONTHEIGHT) THEN
                'Create a watchpoint
                Message$ = "Run until '" + TRIM$(VARIABLES(ContextualMenuLineRef).NAME) + "' (" + TRIM$(VARIABLES(ContextualMenuLineRef).DATATYPE) + ")" + CHR$(LF)
                Message$ = Message$ + "meets the following condition (you can use =, <>, >, >=, <, <=):"
                InitialValue$ = "=" + VARIABLE_DATA(ContextualMenuLineRef).VALUE
                InitialSelection = 1
                IF LEN(TRIM$(WATCHPOINT(ContextualMenuLineRef).EXPRESSION)) > 0 THEN InitialValue$ = TRIM$(WATCHPOINT(ContextualMenuLineRef).EXPRESSION): InitialSelection = -1
                MESSAGEBOX_RESULT = INPUTBOX("Set a watchpoint", Message$, InitialValue$, NewValue$, InitialSelection, -1)
                IF MESSAGEBOX_RESULT = 2 THEN GOTO WatchPointDone
                IF LEN(NewValue$) < 2 THEN
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
                            ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1
                            WATCHPOINT(ContextualMenuLineRef).EXPRESSION = NewValue$
                        CASE ">"
                            IF op2$ = "<" OR op2$ = ">" THEN
                                GOTO WatchpointInvalidExpression
                            END IF
                            ASC(WATCHPOINTLIST, ContextualMenuLineRef) = 1
                            WATCHPOINT(ContextualMenuLineRef).EXPRESSION = NewValue$
                        CASE "<"
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
            ELSEIF (my >= ContextualMenu.Y + 5 + _FONTHEIGHT) AND (my <= ContextualMenu.Y + 5 + _FONTHEIGHT * 2) THEN
                'Edit
                EditVariableRoutine:
                IF INSTR(UCASE$(VARIABLES(ContextualMenuLineRef).SCOPE), GETELEMENT$(CLIENT_CURRENTMODULE, 1) + " " + GETELEMENT$(CLIENT_CURRENTMODULE, 2)) = 0 AND TRIM$(VARIABLES(ContextualMenuLineRef).SCOPE) <> "SHARED" THEN
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
            END IF
        ELSEIF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
            'Click on variable lines
            IF ShowContextualMenu THEN
                ShowContextualMenu = 0
            ELSE
                IF DoubleClick THEN
                    ContextualMenuLineRef = i
                    GOSUB EditVariableRoutine
                    DoubleClick = 0
                END IF
            END IF
        END IF
    END IF

    'Turn on contextual options if right mouse click and while in step mode.
    IF mb2 AND STEPMODE THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(2): _LIMIT 500: SEND_PING: mb2 = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb2 = 0
        IF (my > SCREEN_TOPBAR) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
            'Set contextual menu coordinates relative to this item
            ShowContextualMenu = -1
            ShowPopupWatchpoint = 0
            ContextualMenuYRef = y
            ContextualMenuLineRef = i
            ContextualMenu.printY = printY
            ContextualMenu.FilteredList$ = FilteredList$
            ContextualMenu.W = _PRINTWIDTH(" Set/Edit a watchpoint ") + 6
            ContextualMenu.H = _FONTHEIGHT * 2.5
            ContextualMenu.X = mx: IF ContextualMenu.X + ContextualMenu.W > _WIDTH THEN ContextualMenu.X = _WIDTH - ContextualMenu.W
            ContextualMenu.Y = my: IF ContextualMenu.Y + ContextualMenu.H > _HEIGHT THEN ContextualMenu.Y = _HEIGHT - ContextualMenu.H
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

    longestVarName = 1
    longestScopeSpec = 1
    FOR i = 1 TO TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > longestVarName THEN longestVarName = LEN(TRIM$(VARIABLES(i).NAME))
        IF LEN(TRIM$(VARIABLES(i).SCOPE)) > longestScopeSpec THEN longestScopeSpec = LEN(TRIM$(VARIABLES(i).SCOPE))
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
            IF ctrlDown = -1 THEN y = y - _FONTHEIGHT ELSE y = y - (LIST_AREA / 10)
        CASE 20480 'Down
            IF ctrlDown = -1 THEN y = y + _FONTHEIGHT ELSE y = y + (LIST_AREA / 10)
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
            columnHighlightX = _PRINTWIDTH(SPACE$(longestScopeSpec + 7))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(longestScopeSpec + 28))
            columnHighlightW = _PRINTWIDTH(SPACE$(longestVarName)) + 8
        CASE SCOPE
            columnHighlightX = _PRINTWIDTH(SPACE$(6))
            columnHighlightW = _PRINTWIDTH(SPACE$(longestScopeSpec + 1))
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
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + LEFT$(VARIABLES(i).SCOPE, longestScopeSpec) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName)
            _PRINTSTRING (5, printY), v$
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO TOTALVARIABLES
            printY = ((3 + i) * _FONTHEIGHT) - y
            IF printY > SCREEN_HEIGHT THEN EXIT FOR
            GOSUB ColorizeSelection
            IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + LEFT$(VARIABLES(i).SCOPE, longestScopeSpec) + " " + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName)
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
    DIM insc1%, insc2%
    DIM FoundInclude AS _BIT
    DIM InclResult AS INTEGER
    STATIC CurrDir$

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
                            IncludedFile$ = PATHONLY$(f$) + IncludedFile$
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
    CodeText(Lines&) = readx$
    RETURN
END FUNCTION

'------------------------------------------------------------------------------
SUB PROCESSFILE
    'Parses a .BAS file and reads all compatible variables
    'in order to generate a compatible vWATCH64 client.

    DIM CHECKSUM AS STRING * 8
    DIM CheckingOff AS _BIT
    DIM DeclaringLibrary AS _BIT
    DIM DefaultTypeUsed AS _BIT
    DIM DefiningType AS _BIT
    DIM FoundType AS STRING
    DIM InBetweenSubs AS _BIT
    DIM IsArray AS _BIT
    DIM LocalVariable AS _BIT
    DIM MULTILINE AS _BIT
    DIM MULTILINE_DIM AS _BIT
    DIM MainModule AS _BYTE
    DIM MainModuleEND AS LONG
    DIM NextVar$
    DIM OutputFile AS INTEGER
    DIM PrecompilerBlock AS _BIT
    DIM ProcessLine AS LONG
    DIM ProcessStepDescription AS STRING
    DIM SourceLine AS STRING
    DIM StatusMessage AS STRING
    DIM ThisKeyword AS STRING
    DIM ThisLineHasBPControl AS LONG
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

    IF LEN(TRIM$(NEWFILENAME$)) = 0 THEN
        i = -1
        InputNewFileName:
        DO: _LIMIT 30
            i = i + 1
            IF UCASE$(RIGHT$(FILENAME$, 4)) = ".BAS" THEN
                NEWFILENAME$ = LEFT$(FILENAME$, LEN(FILENAME$) - 4) + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
            ELSE
                NEWFILENAME$ = FILENAME$ + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
            END IF
            IF _FILEEXISTS(NEWFILENAME$) = 0 THEN EXIT DO
        LOOP
    ELSE
        i = -1
        tempFilename$ = NEWFILENAME$
        DO: _LIMIT 30
            i = i + 1
            IF UCASE$(RIGHT$(tempFilename$, 4)) = ".BAS" THEN
                NEWFILENAME$ = LEFT$(tempFilename$, LEN(tempFilename$) - 4) + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
            ELSE
                NEWFILENAME$ = tempFilename$ + IIFSTR$(i > 0, "(" + TRIM$(STR$(i)) + ")", "") + ".vwatch.bas"
            END IF
            IF _FILEEXISTS(NEWFILENAME$) = 0 THEN EXIT DO
        LOOP
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
        MESSAGEBOX_RESULT = MESSAGEBOX("File already exists", Message$, MessageSetup$, 2, 0)
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
                ELSE
                    IF e1$ = "END" AND (e2$ = "FUNCTION" OR e2$ = "SUB") THEN
                        SkipStatement$ = "vWATCH64_DUMMY%% = 0"
                    ELSE
                        SkipStatement$ = "GOTO vwatch64_SKIP_" + LTRIM$(STR$(ProcessLine))
                    END IF
                    IF PrecompilerBlock = 0 AND CheckingOff = 0 AND MULTILINE = 0 THEN
                        IF FirstExecutableLine THEN FirstExecutableLine = 0
                        GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine)) + ":::: " + IIFSTR$(MainModule = 0, "GOSUB vwatch64_VARIABLEWATCH: ", "") + "vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(" + TRIM$(STR$(ProcessLine)) + ", " + IIFSTR$(MainModule = 0, "-1", "0") + "): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN " + SkipStatement$ + " ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine))
                        ThisLineHasBPControl = ProcessLine
                        GOSUB AddNextLineData
                    ELSE
                        IF FirstExecutableLine THEN
                            'Even if all conditions above indicate we shouldn't inject breakpoint
                            'code in this line, we'll inject it anyway if it's the first executable
                            'line we found in the source code, so that we can start paused.
                            FirstExecutableLine = 0
                            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "vwatch64_LABEL_" + LTRIM$(STR$(ProcessLine)) + ":::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(" + TRIM$(STR$(ProcessLine)) + ", " + IIFSTR$(MainModule = 0, "-1", "0") + "): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE"
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

        IF LEFT$(SourceLine, 4) = "DIM " OR (LEFT$(SourceLine, 7) = "STATIC " AND NOT MainModule) THEN
            LocalVariable = 0
            IF MID$(SourceLine, 5, 7) <> "SHARED " THEN LocalVariable = -1

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
                                    'Expand variables defined as UDTs to Variable.Element format:
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
                        FOR i = 1 TO TotalUDTs
                            'Expand variables defined as UDTs to Variable.Element format:
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
                IF INSTR(SourceLine, "(") THEN
                    CurrentSubFunc$ = TRIM$("SUB " + MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5))
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
                REDIM _PRESERVE SUBFUNC_ENDLINE(1 TO TotalSubFunc) AS LONG
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
                IF INSTR(SourceLine, "(") THEN
                    CurrentSubFunc$ = TRIM$("FUNCTION " + MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10))
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
                REDIM _PRESERVE SUBFUNC_ENDLINE(1 TO TotalSubFunc) AS LONG
            ELSE
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
            END IF
        ELSEIF LEFT$(SourceLine, 7) = "END SUB" OR LEFT$(SourceLine, 12) = "END FUNCTION" THEN
            IF INSTR(SourceLine, "END SUB") > 0 THEN
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "EXIT SUB"
            ELSEIF INSTR(SourceLine, "END FUNCTION") > 0 THEN
                GOSUB AddOutputLine: OutputLines(TotalOutputLines) = "EXIT FUNCTION"
            END IF
            GOSUB AddGotoNextLineCode
            GOSUB AddOutputLine: OutputLines(TotalOutputLines) = bkpSourceLine$
            SUBFUNC_ENDLINE(TotalSubFunc) = TotalOutputLines
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
                IF NOT MainModule THEN
                    IF GETELEMENT$(SUBFUNC(CurrSF).NAME, 1) = "FUNCTION" THEN
                        IF NextVar$ = UCASE$(GETELEMENT$(SUBFUNC(CurrSF).NAME, 2)) THEN GOTO NoValidVarFound
                    END IF
                END IF

                'Check if this is actually a CONST:
                'CONST TRUE = -1: CONST FALSE = NOT TRUE
                FoundCONST = FIND_KEYWORD(SourceLine, "CONST", FoundCONSTAt)
                IF FoundCONST AND (FoundCONSTAt < Start) THEN
                    'It's a const.
                    GOTO NoValidVarFound
                END IF

                'All criteria met.
                'Add temporary variable to watchlist: -------------------------
                TempList$ = STRING$(TOTALVARIABLES, 1)
                StartAt = 0
                LookAgain:
                StartAt = StartAt + 1
                Found = FINDVARIABLES(StartAt, NextVar$, TempList$)
                IF Found = 0 THEN
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
            END IF
            NoValidVarFound:
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
    PRINT #OutputFile, "CONST vwatch64_VERSION = " + Q$ + VERSION + Q$
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
    PRINT #OutputFile, "DIM SHARED vwatch64_USERQUIT AS _BIT"
    PRINT #OutputFile, "DIM SHARED vwatch64_NEXTLINE AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_TARGETVARINDEX AS LONG"
    PRINT #OutputFile, "DIM SHARED vwatch64_TIMER AS INTEGER"
    PRINT #OutputFile, "DIM SHARED vwatch64_EXCHANGEDATASIZE$4"
    PRINT #OutputFile, "DIM SHARED vwatch64_EXCHANGEDATA AS STRING"
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
            IF SUBFUNC_ENDLINE(j) = i THEN
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
    PRINT #OutputFile, "FUNCTION vwatch64_CHECKBREAKPOINT&(LineNumber AS LONG, IsSub AS _BYTE)"
    PRINT #OutputFile, "    STATIC FirstRunDone AS _BIT"
    PRINT #OutputFile, "    STATIC StepMode AS _BIT"
    PRINT #OutputFile, "    STATIC StepAround AS _BIT"
    PRINT #OutputFile, "    DIM k AS LONG"
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
    PRINT #OutputFile, "    IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StepAround = -1"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    GOSUB vwatch64_PING"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    'Get the breakpoint list:"
    PRINT #OutputFile, "    vwatch64_BREAKPOINT.ACTION = vwatch64_READY"
    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #OutputFile, "    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTLISTBLOCK, vwatch64_BREAKPOINTLIST"
    PRINT #OutputFile, ""
    PRINT #OutputFile, "    IF StepAround = -1 AND IsSub = -1 AND (ASC(vwatch64_BREAKPOINTLIST, LineNumber) <> 1) THEN EXIT FUNCTION"
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
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StepAround = -1: StepMode = -1"
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
    PRINT #OutputFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StepAround = -1: StepMode = -1"
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
        PRINT #OutputFile, "    DIM i AS LONG"
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
        StatusMessage = Compiler$ + " -x " + Q$ + NOPATH$(NEWFILENAME$) + Q$
        VerboseMaxProgress = 100
        VerboseProgress = 0
        GOSUB AddVerboseOutputLine
        AttemptCompile% = SHELL(ThisPath$ + Compiler$ + " -x " + Q$ + NEWFILENAME$ + Q$)
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
END SUB

'------------------------------------------------------------------------------
FUNCTION CHECKLIST (Text$, List$(), UpperBoundary%)
    'Checks if Text$ is in List$()
    FOR i = 1 TO UpperBoundary%
        IF INSTR(List$(i), Text$) THEN
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
    DIM CurrentPos AS INTEGER
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
    STATIC Position%
    STATIC EndOfStatement AS _BIT

    IF LEN(Text$) = 0 THEN EXIT FUNCTION

    Result$ = ""
    IF EndOfStatement THEN
        EndOfStatement = 0
        GETNEXTVARIABLE$ = ""
        EXIT FUNCTION
    END IF

    IF (WhichLine > 0) AND (WhichLine <> LastLine) THEN
        'First time this line is passed
        Position% = 1
        LastLine = WhichLine

        IF UCASE$(LEFT$(Text$, 4)) = "DIM " THEN
            Position% = 4
            IF MID$(Text$, 5, 7) = "SHARED " THEN Position% = 11
        ELSEIF UCASE$(LEFT$(Text$, 7)) = "STATIC " THEN
            Position% = 7
        END IF
    ELSEIF (WhichLine = -1) THEN
        'Process SUB/FUNCTION parameters instead of DIM variables
        IF LastSF$ <> Text$ THEN
            LastSF$ = Text$
            Position% = INSTR(Text$, "(")
        END IF
    END IF

    IF WhichLine > 0 THEN
        DO
            Position% = Position% + 1
            IF Position% > LEN(Text$) THEN EXIT DO
            Char$ = MID$(Text$, Position%, 1)
            SELECT CASE Char$
                CASE "(": InBrackets = InBrackets + 1
                CASE ")": InBrackets = InBrackets - 1
                CASE ",": IF InBrackets = 0 THEN EXIT DO
                CASE ":": EndOfStatement = -1: EXIT DO
                CASE "_": IF Position% = LEN(Text$) THEN EXIT DO
            END SELECT
            Result$ = Result$ + Char$
        LOOP
    ELSEIF WhichLine = -1 THEN
        DO
            Position% = Position% + 1
            IF Position% > LEN(Text$) THEN EXIT DO
            Char$ = MID$(Text$, Position%, 1)
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
    DIM InsideCheckingOffBlock AS _BIT
    DIM TotalSourceLines AS LONG

    _KEYCLEAR 'Clears the keyboard buffer

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
        IF MENU% = 101 THEN CLOSE #FILE: EXIT SUB
        IF k$ = CHR$(27) THEN USERQUIT = -1
        IF _EXIT THEN USERQUIT = -1
        GOSUB UpdateScreen
    LOOP UNTIL USERQUIT OR HEADER.CONNECTED = -1 OR MENU% = 102

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
    BREAKPOINTLIST = STRING$(CLIENT.TOTALSOURCELINES, 0)
    WATCHPOINTLIST = STRING$(CLIENT.TOTALVARIABLES, 0)
    DATAINFOBLOCK = BREAKPOINTLISTBLOCK + LEN(BREAKPOINTLIST) + 1
    GET #FILE, DATAINFOBLOCK, VARIABLES()
    DATABLOCK = DATAINFOBLOCK + LEN(VARIABLES()) + 1
    WATCHPOINTLISTBLOCK = DATABLOCK + LEN(VARIABLE_DATA()) + 1
    WATCHPOINTEXPBLOCK = WATCHPOINTLISTBLOCK + LEN(WATCHPOINTLIST) + 1
    WATCHPOINTCOMMANDBLOCK = WATCHPOINTEXPBLOCK + LEN(WATCHPOINT()) + 1
    EXCHANGEBLOCK = WATCHPOINTCOMMANDBLOCK + LEN(WATCHPOINT_COMMAND) + 1
    LINE_TRAIL = LEN("[ ]  " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN("1")) + "1" + "    ")

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
            TotalSubFunc = 0
            FOR i = 1 TO TotalSourceLines
                bkpSourceLine$ = SOURCECODE(i)
                caseBkpSourceLine$ = TRIM$(bkpSourceLine$)
                SourceLine$ = UCASE$(TRIM$(caseBkpSourceLine$))
                IF SourceLine$ = "$CHECKING:OFF" OR LEFT$(SourceLine$, 13) = "'VWATCH64:OFF" THEN
                    InsideCheckingOffBlock = -1
                END IF
                IF InsideCheckingOffBlock THEN ASC(CHECKINGOFF_LINES, i) = 1
                IF SourceLine$ = "$CHECKING:ON" OR LEFT$(SourceLine$, 12) = "'VWATCH64:ON" THEN
                    InsideCheckingOffBlock = 0
                END IF
                IF LEN(bkpSourceLine$) > LONGESTLINE THEN LONGESTLINE = LEN(bkpSourceLine$)

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
    DO: _LIMIT 500
        mx = _MOUSEX
        my = _MOUSEY
        mb = _MOUSEBUTTON(1)
    LOOP WHILE _MOUSEINPUT
    RETURN

    UpdateScreen:
    CLS , _RGB32(255, 255, 255)
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 5), _RGB32(0, 178, 179), BF

    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Waiting for a connection..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$
    t$ = "Launch the program that will be monitored now"
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT - _FONTHEIGHT * 2), t$
    t$ = "ESC to quit"
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
                IF INSTR(Buttons(cb).CAPTION, "$INCLUDE") THEN MENU% = 103: RETURN
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
FUNCTION SelectFile$ (search$, x AS INTEGER, y AS INTEGER)
    'Steve McNeill's File Selection Utility v1.2
    'http://www.qb64.net/forum/index.php?topic=11253.0

    'save some old values
    LoadFile_DC = _DEFAULTCOLOR: LoadFile_BG = _BACKGROUNDCOLOR
    LoadFile_s = _SOURCE: LoadFile_d = _DEST
    'f = _FONT: _FONT 16
    'some variables

    LoadFile_BoxColor = &HFFAAAAFF
    LoadFile_FolderColor = &HFFFFFF00
    LoadFile_FileColor = &HFFFFFFFF
    IF INSTR(_OS$, "[WINDOWS]") THEN LoadFile_Slash$ = "\" ELSE LoadFile_Slash$ = "/"
    LoadFile_Dir$ = SPACE$(FILE_current_dir_length)
    FILE_get_current_dir LoadFile_Dir$
    LoadFile_Dir$ = LoadFile_Dir$ + LoadFile_Slash$
    LoadFile_w = 639: LoadFile_h = 479
    REDIM LoadFile_Label(0) AS STRING: LoadFile_Label(0) = "DIR"
    REDIM LoadFile_DirList(-1 TO 9, -1 TO 9999) AS STRING
    LoadFile_last = 1
    REDIM Drives(0) AS STRING

    'some error checking
    IF search$ = "" THEN EXIT SUB 'We can't search for nothing!

    'Copy background
    PCOPY 0, 1
    'set workscreen
    LoadFile_ws = _NEWIMAGE(640, 480, 32)

    'Count our filetypes to display
    LoadFile_TypeCount = 0
    DO
        LoadFile_TypeCount = LoadFile_TypeCount + 1
        LoadFile_l = INSTR(LoadFile_l + 1, search$, ";") ' look for ; to denote more files
        REDIM _PRESERVE LoadFile_Label(LoadFile_TypeCount) AS STRING
        IF LoadFile_l > 0 THEN LoadFile_Label(LoadFile_TypeCount) = MID$(search$, LoadFile_last + 1, LoadFile_l - LoadFile_last - 1) ELSE LoadFile_Label(LoadFile_TypeCount) = MID$(search$, LoadFile_last + 1, LEN(search$) - LoadFile_last)
        LoadFile_last = LoadFile_l + 1
    LOOP UNTIL LoadFile_l = 0
    LoadFile_l = 640 / (LoadFile_TypeCount + 1)
    REDIM LoadFile_start(LoadFile_TypeCount), LoadFile_previous(LoadFile_TypeCount), LoadFile_more(LoadFile_TypeCount), LoadFile_Count(LoadFile_TypeCount)
    FOR i = 0 TO LoadFile_TypeCount: LoadFile_start(i) = 1: NEXT

    'Get the windows drive letters
    IF INSTR(_OS$, "[WINDOWS]") THEN
        SHELL _HIDE CHR$(34) + "wmic logicaldisk get name" + CHR$(34) + ">TempDirList.txt"
        REDIM Drives(0) AS STRING

        OPEN "TempDirList.txt" FOR INPUT AS #1
        LINE INPUT #1, junk$ 'First line is  name
        counter = 0
        DO UNTIL EOF(1)
            counter = counter + 1
            INPUT #1, junk$ 'drive name
            REDIM _PRESERVE Drives(counter) AS STRING
            IF LEN(junk$) > 1 THEN junk$ = MID$(junk$, 2, 1) + ":" ELSE junk$ = "": counter = counter - 1
            IF junk$ <> "" THEN
                Drives(counter) = junk$
            END IF
        LOOP
        CLOSE #1
        KILL "TempDirList.txt"
    END IF

    _SOURCE LoadFile_ws: _DEST LoadFile_ws
    IF TTFONT > 0 AND NO_TTFONT = 0 THEN _FONT TTFONT
    DO

        FOR i = 0 TO LoadFile_TypeCount
            LoadFile_Count(i) = 0
            FOR j = 0 TO 9999
                LoadFile_DirList(i, j) = ""
            NEXT
        NEXT
        'Generate our updated directory listings.

        IF FILE_load_dir&(LoadFile_Dir$ + CHR$(0)) THEN
            DO
                LoadFile_length = FILE_has_next_entry 'Get length of next entry
                IF LoadFile_length > -1 THEN 'If we have a next entry
                    LoadFile_nam$ = SPACE$(LoadFile_length) 'Set the size of our string
                    FILE_get_next_entry LoadFile_nam$, LoadFile_flags, LoadFile_file_size 'Get the file's name, size, and 'flags'
                    'Check if it's a file or a directory

                    IF _DIREXISTS(LoadFile_Dir$ + LoadFile_nam$) THEN
                        IF LoadFile_nam$ <> "." THEN
                            LoadFile_Count(0) = LoadFile_Count(0) + 1
                            LoadFile_DirList(0, LoadFile_Count(0)) = LoadFile_nam$
                        END IF
                    ELSE 'We have a file
                        FOR i = 1 TO LoadFile_TypeCount
                            LoadFile_ext$ = RIGHT$(LoadFile_nam$, LEN(LoadFile_Label(i)))
                            IF UCASE$(LoadFile_ext$) = UCASE$(LoadFile_Label(i)) THEN
                                LoadFile_Count(i) = LoadFile_Count(i) + 1
                                LoadFile_DirList(i, LoadFile_Count(i)) = LoadFile_nam$
                                EXIT FOR
                            ELSEIF LoadFile_Label(i) = ".*" THEN
                                LoadFile_Count(i) = LoadFile_Count(i) + 1
                                LoadFile_DirList(i, LoadFile_Count(i)) = LoadFile_nam$
                            END IF
                        NEXT
                    END IF
                END IF
            LOOP UNTIL LoadFile_length = -1
            FILE_close_dir
        END IF

        FOR i = 1 TO UBOUND(drives)
            LoadFile_Count(0) = LoadFile_Count(0) + 1
            LoadFile_DirList(0, LoadFile_Count(0)) = Drives(i)
        NEXT

        updatelist:

        CLS , &HFF005050 'Draw a nice display box
        COLOR , 0
        LINE (0, 0)-(LoadFile_w, LoadFile_h + 5 - 2 * 16), LoadFile_BoxColor, B
        LINE (1, 1)-(LoadFile_w - 1, LoadFile_h + 6 - 2 * 16), LoadFile_BoxColor, B
        LINE (0, 0)-(LoadFile_w, LoadFile_h), LoadFile_BoxColor, B
        LINE (1, 1)-(LoadFile_w - 1, LoadFile_h - 1), LoadFile_BoxColor, B

        LINE (0, 16 + 3)-(LoadFile_w, 16 + 3), LoadFile_BoxColor
        LINE (0, 16 + 4)-(LoadFile_w, 16 + 4), LoadFile_BoxColor
        FOR i = 0 TO LoadFile_TypeCount
            _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * LEN(LoadFile_Label(i))) / 2, 2), LoadFile_Label(i)
            LINE (i * LoadFile_l, 0)-(i * LoadFile_l, LoadFile_h + 5 - 2 * 16), LoadFile_BoxColor
        NEXT

        LINE (627, 2)-(637, 18), &HFFFF0000, BF
        LINE (626, 2)-(637, 18), &HFF000000, B

        _PRINTSTRING (628, 2), "X"
        IF selection > 0 THEN
            temp$ = LoadFile_DirList(row, selection)
            IF LoadFile_DirList(row, selection) = "" THEN temp$ = ""
            selection = 0
        END IF
        _PRINTSTRING (10, 28 * 16 + 7), LoadFile_Dir$
        _PRINTSTRING (630 - LEN(temp$) * 8, 28 * 16 + 7), temp$
        IF temp$ = "" THEN oldselection = 0
        IF oldselection > 0 THEN LINE (row * LoadFile_l, (oldselection + 1) * 16 + 5)-((row + 1) * LoadFile_l, (oldselection + 2) * 16 + 5), &HAAAAA000, BF

        FOR i = 0 TO UBOUND(LoadFile_label)
            IF i = 0 THEN COLOR LoadFile_FolderColor ELSE COLOR LoadFile_FileColor
            counter = 0
            FOR j = LoadFile_start(i) TO LoadFile_start(i) + 24
                counter = counter + 1
                IF LoadFile_DirList(i, j) = "" THEN EXIT FOR
                _PRINTSTRING (i * LoadFile_l + 5, (counter + 1) * 16 + 7), LEFT$(LoadFile_DirList(i, j), LoadFile_l / 8 - 2)
            NEXT
            IF j = LoadFile_start(i) + 25 THEN LoadFile_more(i) = -1 ELSE LoadFile_more(i) = 0
            IF LoadFile_start(i) > 1 THEN LoadFile_previous(i) = -1 ELSE LoadFile_previous(i) = 0
            IF LoadFile_more(i) THEN
                LINE (i * LoadFile_l + 2, 27 * 16 + 5)-((i + 1) * LoadFile_l - 3, 28 * 16 + 3), &HFFFF0000, BF
                LINE (i * LoadFile_l + 2, 27 * 16 + 5)-((i + 1) * LoadFile_l - 3, 28 * 16 + 3), BoxColor, B
                COLOR &HFFFFFF00: _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * 11) / 2, 27 * 16 + 5), "SCROLL DOWN"
                COLOR LoadFile_FileColor
            END IF
            IF LoadFile_previous(i) THEN
                LINE (i * LoadFile_l + 2, 16 + 5)-((i + 1) * LoadFile_l - 3, 2 * 16 + 3), &HFFFF0000, BF
                LINE (i * LoadFile_l + 2, 16 + 5)-((i + 1) * LoadFile_l - 3, 2 * 16 + 3), BoxColor, B
                COLOR &HFFFFFF00: _PRINTSTRING (i * LoadFile_l + (LoadFile_l - 8 * 9) / 2, 16 + 5), "SCROLL UP"
                COLOR LoadFile_FileColor
            END IF
        NEXT

        _PUTIMAGE (0 + x, 0 + y)-(640 + x, 480 + y), LoadFile_ws, 0
        _DISPLAY

        change = 0
        DO
            _DELAY .05
            LoadFile_LMB = 0 'This sets the left mouse button as unacceptable.
            a = _KEYHIT
            SELECT CASE a
                CASE 8 'backspace
                    temp$ = LEFT$(temp$, LEN(temp$) - 1)
                    change = -1
                CASE 13 'enter
                    DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer so it doesn't affect the main program.
                    temp$ = LoadFile_Dir$ + temp$
                    COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = temp$ 'Restore our old settings
                    '_FONT f
                    EXIT SUB 'And leave
                CASE 27 'If ESC is pressed then...
                    DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer so it doesn't affect the main program.
                    COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = "" 'Restore our old settings
                    '_FONT f
                    EXIT SUB 'And leave
                CASE 32 TO 126
                    temp$ = temp$ + CHR$(a)
                    change = -1
            END SELECT
            DO
                MS = MS + _MOUSEWHEEL
                IF _MOUSEBUTTON(1) = 0 THEN LoadFile_LMB = -1 'Only by lifting the mouse, will we count it as down
                'Note: we ignore LoadFile_LMB for the scroll bars, so we can just hold it down and scroll happily forever and ever...
                'or until we get to the limit of our file list.
                'We only check LoadFile_LMB when actually trying to select an item from our list.   No more "OOP!  I held it too long and did something I didn't want to do!"
                'Now we click once to select, click again to accept that selection.
            LOOP WHILE _MOUSEINPUT
            MX = _MOUSEX: MY = _MOUSEY
            IF _MOUSEBUTTON(2) OR (LoadFile_LMB AND MX > 626 + x AND MX < 638 + x AND MY > 1 + y AND MY < 19 + y AND _MOUSEBUTTON(1)) THEN
                'restore those old values, and just exit.  Right mouse is an escape
                COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = ""
                '_FONT f
                EXIT SUB
            END IF





            IF _MOUSEBUTTON(1) THEN 'Without the mouse being down, we don't need to check squat!
                'Check the 2 roLoadFile_ws for a click in the proper Y position
                IF MY >= 16 + 5 + y AND MY <= 2 * 16 + 3 + y THEN 'We're on the top row
                    FOR j = 0 TO UBOUND(LoadFile_label)
                        IF LoadFile_previous(j) AND MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                            LoadFile_start(j) = LoadFile_start(j) - 1
                            change = -1: selection = 0: click = 0: temp$ = ""
                            EXIT FOR
                        END IF
                    NEXT
                ELSEIF MY >= 27 * 16 + 5 + y AND MY <= 28 * 16 + 3 + y THEN 'We're on the bottom row
                    FOR j = 0 TO UBOUND(LoadFile_label)
                        IF LoadFile_more(j) AND MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                            LoadFile_start(j) = LoadFile_start(j) + 1
                            change = -1: selection = 0: click = 0: temp$ = ""
                            EXIT FOR
                        END IF
                    NEXT
                ELSEIF MY >= 37 + y AND MY <= 437 + y AND LoadFile_LMB THEN 'It's in a column somewhere.  Did someone click an item?!
                    FOR j = 0 TO UBOUND(LoadFile_label)
                        IF MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                            row = j
                            oldselection = INT((MY - y - 37) / 16) + 1
                            selection = LoadFile_start(j) + oldselection - 1
                            change = -1
                            click = -1
                            EXIT FOR
                        END IF
                    NEXT
                END IF
            END IF
            IF MS <> 0 THEN
                IF MY >= 37 + y AND MY <= 437 + y AND LoadFile_LMB THEN 'It's in a column somewhere.  Did someone click an item?!
                    FOR j = 0 TO UBOUND(LoadFile_label)
                        IF MX >= j * LoadFile_l + 2 + x AND MX <= (j + 1) * LoadFile_l - 3 + x THEN
                            IF LoadFile_previous(j) AND MS < 1 THEN
                                LoadFile_start(j) = LoadFile_start(j) - 5
                                IF LoadFile_start(j) < 1 THEN LoadFile_start(j) = 1
                                change = -1: selection = 0: click = 0: temp$ = ""
                                MS = 0
                            ELSEIF LoadFile_more(j) AND MS > 1 THEN
                                LoadFile_start(j) = LoadFile_start(j) + 5
                                change = -1: selection = 0: click = 0: temp$ = ""
                                MS = 0
                            END IF
                            EXIT FOR
                        END IF
                    NEXT
                ELSE MS = 0
                END IF
            END IF
            _DISPLAY
        LOOP UNTIL change
        IF click THEN 'we clicked something besides a scroll bar
            temp1$ = LoadFile_DirList(row, selection)
            IF temp$ = temp1$ THEN
                'We picked one!
                SELECT CASE LoadFile_Label(row)
                    CASE "DIR"
                        SELECT CASE LoadFile_DirList(row, selection)
                            CASE "" 'Do nothing with blank directories
                            CASE ".." 'Up a folder
                                DO
                                    LoadFile_Dir$ = LEFT$(LoadFile_Dir$, LEN(LoadFile_Dir$) - 1)
                                LOOP UNTIL RIGHT$(LoadFile_Dir$, 1) = LoadFile_Slash$ OR LEN(LoadFile_Dir$) = 0
                            CASE ELSE 'To a specific folder
                                IF LEN(LoadFile_DirList(row, selection)) = 2 AND RIGHT$(LoadFile_DirList(row, selection), 1) = ":" THEN
                                    'It's a directory change
                                    LoadFile_Dir$ = LoadFile_DirList(row, selection) + LoadFile_Slash$
                                ELSE
                                    LoadFile_Dir$ = LoadFile_Dir$ + LoadFile_DirList(row, selection) + LoadFile_Slash$
                                END IF
                        END SELECT
                        FOR i = 0 TO UBOUND(Loadfile_start)
                            LoadFile_start(i) = 1
                        NEXT
                        selection = 0: temp$ = "": oldselection = 0
                    CASE ".*": SelectFile$ = LoadFile_Dir$ + temp$: EXIT DO
                    CASE ELSE: SelectFile$ = LoadFile_Dir$ + temp$: EXIT DO
                END SELECT
            END IF
            IF row > 0 THEN _DELAY .2: GOTO updatelist
        ELSE
            _DELAY .05
            GOTO updatelist
        END IF
    LOOP
    'restore those old values
    COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY
    '_FONT f
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

    IF LEFT$(T$, S.L) = S$ AND INSTR(SEP$, MID$(T$, S.L + 1, 1)) > 0 THEN
        SearchTermFound = SearchTermFound + T.L
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
        ELSEIF my >= SCREEN_HEIGHT - 21 THEN
            LINE (_WIDTH - 30, SCREEN_HEIGHT - 21)-STEP(29, 20), _RGBA32(230, 230, 230, 235), BF
        END IF
    END IF
    _PRINTSTRING (_WIDTH - 20, SCREEN_TOPBAR + 5), CHR$(24)
    _PRINTSTRING (_WIDTH - 20, SCREEN_HEIGHT - _FONTHEIGHT - 5), CHR$(25)

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
    'Resize routine adapted from Steve McNeill:
    'http://www.qb64.net/forum/index.php?topic=11053.msg93650#msg93650

    DIM ts AS LONG 'a temp screen
    DIM dc AS LONG, bg AS LONG 'default and background colors
    dc = _DEFAULTCOLOR: bg = _BACKGROUNDCOLOR

    IF _RESIZE = 0 THEN
        IF new_w% + new_h% = 0 THEN EXIT SUB
    ELSE
        new_w% = _RESIZEWIDTH
        new_h% = _RESIZEHEIGHT
    END IF

    IF new_w% = SCREEN_WIDTH AND new_h% = SCREEN_HEIGHT THEN EXIT SUB

    IF new_w% < DEFAULT_WIDTH THEN new_w% = DEFAULT_WIDTH
    IF new_h% < SCREEN_TOPBAR * 3 THEN new_h% = SCREEN_TOPBAR * 3

    SCREEN_WIDTH = new_w%
    SCREEN_HEIGHT = new_h%

    ts = _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
    _PUTIMAGE , MAINSCREEN, ts

    SCREEN ts
    _FREEIMAGE MAINSCREEN
    MAINSCREEN = _NEWIMAGE(new_w%, new_h%, 32)
    _PUTIMAGE (0, 0)-(_WIDTH - 1, _HEIGHT - 1), ts, MAINSCREEN
    SCREEN MAINSCREEN
    COLOR dc, bg
    _FREEIMAGE ts

    LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR
    SB_TRACK = LIST_AREA - 48

    $IF WIN THEN
        IF TTFONT > 0 AND NO_TTFONT = 0 THEN _FONT TTFONT
    $END IF
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

