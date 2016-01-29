'vWATCH64 - A debug/variable watch system for QB64 programs
'Fellippe Heitor, 2015/2016 - fellippeheitor@gmail.com - @fellippeheitor

DEFLNG A-Z
$RESIZE:ON

$IF WIN THEN
    DECLARE LIBRARY
        FUNCTION GetModuleFileNameA (BYVAL hModule AS LONG, lpFileName AS STRING, BYVAL nSize AS LONG)
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

'$INCLUDE:'glinputtop.bi'

'Constants: -------------------------------------------------------------------
CONST ID = "vWATCH64"
CONST VERSION = ".951b"

CONST FALSE = 0
CONST TRUE = NOT FALSE

CONST TIMEOUTLIMIT = 3 'SECONDS

'Breakpoint control:
CONST CONTINUE = 1
CONST NEXTSTEP = 2
CONST READY = 3

'Filters:
CONST VARIABLENAMES = 1
CONST VALUES = 2
CONST SCOPE = 3
CONST DATATYPES = 4
CONST CODE = 5
CONST LINENUMBERS = 6

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
    HOST_PING AS _BYTE
    CLIENT_PING AS _BYTE
    HISTORY_LOG AS _BYTE
END TYPE

TYPE CLIENTTYPE
    NAME AS STRING * 256
    CHECKSUM AS STRING * 8
    TOTALSOURCELINES AS LONG
    EXENAME AS STRING * 256
    CURRENTMODULE AS STRING * 50
    LINENUMBER AS LONG
    TOTALVARIABLES AS LONG
END TYPE

TYPE BREAKPOINTTYPE
    ACTION AS _BYTE
END TYPE

TYPE VARIABLESTYPE
    NAME AS STRING * 256
    SCOPE AS STRING * 7
    UDT AS STRING * 40
    DATATYPE AS STRING * 20
    VALUE AS STRING * 255
END TYPE

TYPE UDTTYPE
    UDT AS STRING * 40
    ELEMENT AS STRING * 256
    DATATYPE AS STRING * 20
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
DIM SHARED DEFAULTDATATYPE(65 TO 90) AS STRING * 20
DIM SHARED EXENAME AS STRING
DIM SHARED FILE AS INTEGER
DIM SHARED FILENAME$
DIM SHARED FILEERRORRAISED AS _BIT
DIM SHARED PAGE_HEIGHT AS LONG
DIM SHARED INTERNALKEYWORDS AS INTEGER
DIM SHARED LAST_PING#
DIM SHARED LF AS _BYTE
DIM SHARED LIST_AREA AS INTEGER
DIM SHARED LONGESTLINE AS LONG
DIM SHARED MAINSCREEN AS LONG
DIM SHARED MENU%
DIM SHARED NEWFILENAME$
DIM SHARED SB_TRACK AS INTEGER
DIM SHARED SCREEN_WIDTH AS INTEGER
DIM SHARED SCREEN_HEIGHT AS INTEGER
DIM SHARED SET_OPTIONBASE AS INTEGER
DIM SHARED SOURCEFILE AS STRING
DIM SHARED TITLESTRING AS STRING
DIM SHARED TOTALBREAKPOINTS AS LONG
DIM SHARED TOTALVARIABLES AS LONG
DIM SHARED TTFONT AS LONG
DIM SHARED hWnd&

'File structure:
DIM SHARED BREAKPOINTBLOCK AS LONG
DIM SHARED BREAKPOINTLISTBLOCK AS LONG
DIM SHARED CLIENTBLOCK AS LONG
DIM SHARED DATABLOCK AS LONG
DIM SHARED HEADERBLOCK AS LONG

'Custom structures:
DIM SHARED BREAKPOINT AS BREAKPOINTTYPE
DIM SHARED CLIENT AS CLIENTTYPE
DIM SHARED HEADER AS HEADERTYPE

'Switches:
DIM SHARED DONTCOMPILE AS _BIT
DIM SHARED FIRSTPROCESSING AS _BIT
DIM SHARED INTERACTIVE AS _BIT
DIM SHARED MULTILINE AS _BIT
DIM SHARED NO_TTFONT AS _BIT
DIM SHARED STEPMODE AS _BIT
DIM SHARED SKIPARRAYS AS _BIT
DIM SHARED TIMED_OUT AS _BIT
DIM SHARED USERQUIT AS _BIT
DIM SHARED CLOSE_SESSION AS _BIT
DIM SHARED VERBOSE AS _BIT

REDIM SHARED VARIABLES(0) AS VARIABLESTYPE
REDIM SHARED LINE_STARTS(0) AS LONG

DIM OVERLAYSCREEN AS LONG
DIM i AS INTEGER

'Variables initialization: ----------------------------------------------------
FOR i = 65 TO 90
    DEFAULTDATATYPE(i) = "SINGLE"
NEXT i
SET_OPTIONBASE = 0
VERBOSE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0
INTERACTIVE = 0
NO_TTFONT = 0
FIRSTPROCESSING = -1
SCREEN_WIDTH = DEFAULT_WIDTH
SCREEN_HEIGHT = DEFAULT_HEIGHT
LIST_AREA = SCREEN_HEIGHT - SCREEN_TOPBAR
SB_TRACK = LIST_AREA - 48

RESTORE_LIBRARY

'Screen setup: ----------------------------------------------------------------
MAINSCREEN = _NEWIMAGE(SCREEN_WIDTH, SCREEN_HEIGHT, 32)
SCREEN MAINSCREEN
TITLESTRING = "vWATCH64 - v" + VERSION
_TITLE TITLESTRING

'Parse the command line: ------------------------------------------------------
'Did the user drag a .BAS file onto this program or enter parameters?
'Syntax: VWATCH64 [source filename.bas] [-options] [-target [newfilename]]
'If no parameters are passed, vWATCH64 starts in MONITOR MODE
IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT > 1 THEN
        'Set flags based on command line arguments:
        FOR i = 1 TO _COMMANDCOUNT
            SELECT CASE LCASE$(COMMAND$(i))
                CASE "-verbose", "-v": VERBOSE = -1
                CASE "-dontcompile", "-d": DONTCOMPILE = -1
                CASE "-noarrays", "-n": SKIPARRAYS = -1
                CASE "-interactive", "-i": INTERACTIVE = -1
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
    IF NO_TTFONT = 0 THEN TTFONT = _LOADFONT("C:\windows\fonts\lucon.ttf", 14, "MONOSPACE, BOLD")
    IF TTFONT > 0 AND NO_TTFONT = 0 THEN _FONT TTFONT

    Ret = GetModuleFileNameA(0, EXENAME_HOLDER$256, LEN(EXENAME_HOLDER$256))
    IF Ret > 0 THEN
        EXENAME = LEFT$(EXENAME_HOLDER$256, Ret)
    END IF
$ELSE
    EXENAME = ""
$END IF

IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT = 1 AND NO_TTFONT = 0 THEN
        IF _FILEEXISTS(COMMAND$(1)) THEN FILENAME$ = COMMAND$(1): PROCESSFILE ELSE BEEP
        NEWFILENAME$ = "": FIRSTPROCESSING = 0
    ELSEIF _COMMANDCOUNT > 1 THEN
        IF _FILEEXISTS(COMMAND$(1)) THEN FILENAME$ = COMMAND$(1): PROCESSFILE ELSE BEEP
        NEWFILENAME$ = "": FIRSTPROCESSING = 0
    END IF
END IF

GOTO MainLoop
OpenFileMenu:
IF SCREEN_WIDTH < DEFAULT_WIDTH OR SCREEN_HEIGHT < DEFAULT_HEIGHT THEN CHECK_RESIZE DEFAULT_WIDTH, DEFAULT_HEIGHT
_RESIZE OFF
CLS , _RGB32(255, 255, 255)
_PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
FILENAME$ = SelectFile$("*.BAS;*.*", _WIDTH(MAINSCREEN) / 2 - 320, _HEIGHT(MAINSCREEN) / 2 - 240)
_RESIZE ON
_AUTODISPLAY

'Reset flags:
FOR i = 65 TO 90
    DEFAULTDATATYPE(i) = "SINGLE"
NEXT i
SET_OPTIONBASE = 0
VERBOSE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0
INTERACTIVE = 0
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

KeyWordsDATA:
DATA _BIT,_UNSIGNED _BIT,_BYTE,_UNSIGNED _BYTE,INTEGER
DATA _UNSIGNED INTEGER,LONG,_UNSIGNED LONG,_INTEGER64
DATA _UNSIGNED _INTEGER64,SINGLE,DOUBLE,_FLOAT,STRING
DATA END

'------------------------------------------------------------------------------
'SUBs and FUNCTIONs:                                                          -
'------------------------------------------------------------------------------
SUB SOURCE_VIEW
    'Allows setting breakpoints and stepping through code
    DIM SB_Ratio AS SINGLE
    DIM SourceLine AS STRING
    DIM ListEnd_Label AS STRING
    STATIC SearchIn

    TotalButtons = 6
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    TOTALBREAKPOINTS = 0
    BREAKPOINT.ACTION = 0 'Start paused; execution starts with F5 or F8.
    PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS , _RGB32(255, 255, 255)

    Filter$ = ""
    IF SearchIn = 0 THEN SearchIn = CODE
    SB_ThumbY = 0
    grabbedY = -1
    ListEnd_Label = "(end of source file)"
    STEPMODE = -1
    TRACE = -1
    _KEYCLEAR
    TIMED_OUT = 0
    CLOSE_SESSION = 0

    DO: _LIMIT 500
        GOSUB ProcessInput
        GET #FILE, CLIENTBLOCK, CLIENT
        IF CLIENT.LINENUMBER <> prev_LineNumber AND ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
            STEPMODE = -1
            TRACE = -1
            prev_LineNumber = CLIENT.LINENUMBER
        END IF
        PUT #FILE, BREAKPOINTLISTBLOCK, BREAKPOINTLIST
        GOSUB UpdateList
        IF _EXIT THEN USERQUIT = -1
        SEND_PING
    LOOP UNTIL HEADER.CONNECTED = 0 OR USERQUIT OR TIMED_OUT OR CLOSE_SESSION

    EndMessage:
    _AUTODISPLAY
    IF USERQUIT THEN EXIT SUB

    IF CLOSE_SESSION THEN
        HEADER.CONNECTED = 0
        PUT #FILE, HEADERBLOCK, HEADER
        CLOSE #FILE
        ON ERROR GOTO FileError
        KILL PATHONLY$(EXENAME) + "vwatch64.dat"
        ON ERROR GOTO 0
        EXIT SUB
    END IF

    IF HEADER.CONNECTED = 0 THEN
        EndMessage$ = "Connection closed by client."
    ELSEIF TIMED_OUT THEN
        EndMessage$ = "Connection timed out."
    END IF

    IF HEADER.CONNECTED = 0 OR TIMED_OUT THEN
        OVERLAYSCREEN = _NEWIMAGE(SCREEN_WIDTH \ 2, SCREEN_HEIGHT \ 2, 32)
        _DEST OVERLAYSCREEN

        IF TTFONT > 0 AND NO_TTFONT = 0 THEN _FONT TTFONT
        LINE (0, 0)-STEP(799, 599), _RGBA32(255, 255, 255, 200), BF
        BEEP
        _KEYCLEAR
        COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
        _PRINTSTRING ((_WIDTH / 2 - _PRINTWIDTH(EndMessage$) / 2) + 1, (_HEIGHT / 2 - _FONTHEIGHT / 2) + 1), EndMessage$
        COLOR _RGB32(255, 255, 255), _RGBA32(0, 0, 0, 0)
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(EndMessage$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), EndMessage$
        EndMessage$ = "Press any key to continue..."
        COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
        _PRINTSTRING ((_WIDTH / 2 - _PRINTWIDTH(EndMessage$) / 2) + 1, (_HEIGHT / 2 - _FONTHEIGHT / 2) + _FONTHEIGHT + 1), EndMessage$
        COLOR _RGB32(255, 255, 255), _RGBA32(0, 0, 0, 0)
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(EndMessage$) / 2, (_HEIGHT / 2 - _FONTHEIGHT / 2) + _FONTHEIGHT), EndMessage$
        _DEST MAINSCREEN
        _PUTIMAGE , OVERLAYSCREEN
        _FREEIMAGE OVERLAYSCREEN
        DO: _LIMIT 30
            WHILE _MOUSEINPUT: mb = _MOUSEBUTTON(1): WEND
            IF mb THEN EXIT DO
            IF _EXIT THEN USERQUIT = -1: EXIT DO
        LOOP UNTIL _KEYHIT
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
    LOOP WHILE _MOUSEINPUT

    SELECT EVERYCASE k
        CASE 32 TO 126 'Printable ASCII characters
            SELECT CASE SearchIn
                CASE CODE
                    Filter$ = Filter$ + CHR$(k)
                CASE LINENUMBERS
                    IF (k >= 48 AND k <= 57) OR (k = 45) OR (k = 44) THEN Filter$ = Filter$ + CHR$(k)
            END SELECT
        CASE 8 'Backspace
            IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
        CASE 9 'TAB
            IF SearchIn = CODE THEN SearchIn = LINENUMBERS ELSE SearchIn = CODE
            IF LEN(Filter$) > 0 AND VAL(Filter$) = 0 AND SearchIn = LINENUMBERS THEN Filter$ = ""
        CASE 27 'ESC clears the current search filter or exits interactive mode
            ExitButton_Click:
            IF LEN(Filter$) THEN
                Filter$ = ""
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
            STEPMODE = 0
            BREAKPOINT.ACTION = CONTINUE
            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16384 'F6
            WindowButton_Click:
            IF CLIENT.TOTALVARIABLES > 0 THEN
                _KEYCLEAR
                GET #FILE, DATABLOCK, VARIABLES()
                VARIABLE_VIEW
            END IF
            IF Clicked THEN Clicked = 0: RETURN
        CASE 16896 'F8
            StepButton_Click:
            STEPMODE = -1
            TRACE = -1
            BREAKPOINT.ACTION = NEXTSTEP
            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
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
            ELSE
                FOR setAll = 1 TO LEN(FilteredList$) / 4
                    which_line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_line) = 0 AND LEN(STRIPCOMMENTS$(GETLINE$(which_line))) THEN
                        ASC(BREAKPOINTLIST, which_line) = 1
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
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
                    which_line = CVL(MID$(FilteredList$, setAll * 4 - 3, 4))
                    IF ASC(BREAKPOINTLIST, which_line) = 1 THEN
                        ASC(BREAKPOINTLIST, which_line) = 0
                        TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                    END IF
                NEXT setAll
            END IF
            IF Clicked THEN Clicked = 0: RETURN
    END SELECT

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

    'Build a filtered list, if a filter is active:
    i = 0: FilteredList$ = ""
    PAGE_HEIGHT = _FONTHEIGHT * (CLIENT.TOTALSOURCELINES + 3)
    IF LEN(Filter$) > 0 THEN
        DO
            i = i + 1
            IF i > CLIENT.TOTALSOURCELINES THEN EXIT DO
            IF SearchIn = CODE THEN Found = MULTI_SEARCH(UCASE$(GETLINE$(i)), UCASE$(Filter$))
            IF SearchIn = LINENUMBERS THEN Found = INTERVAL_SEARCH(Filter$, i)
            IF Found THEN
                FilteredList$ = FilteredList$ + MKL$(i)
            END IF
        LOOP
        IF LEN(FilteredList$) > 0 THEN PAGE_HEIGHT = _FONTHEIGHT * ((LEN(FilteredList$) / 4) + 3)
    END IF

    'Scroll to the next line of code that will be run
    IF TRACE THEN
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
    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
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
                v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), " ") + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
                _PRINTSTRING (5, printY), v$
                COLOR _RGB32(0, 0, 0)
            END IF
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        ListStart = ((y \ _FONTHEIGHT) + 1)
        ListEnd = CLIENT.TOTALSOURCELINES
        FOR i = ListStart TO ListEnd
            SourceLine = GETLINE$(i)
            printY = (SCREEN_TOPBAR + 3 + ((i - 1) * _FONTHEIGHT)) - y
            IF printY > SCREEN_HEIGHT THEN EXIT FOR
            'Print only inside the program area
            GOSUB ColorizeList
            IF (my > SCREEN_TOPBAR + 1) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            v$ = "[" + IIFSTR$(ASC(BREAKPOINTLIST, i) = 1, CHR$(7), " ") + "]" + IIFSTR$(i = CLIENT.LINENUMBER, CHR$(16) + " ", "  ") + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(i)))) + TRIM$(STR$(i)) + "    " + SourceLine
            _PRINTSTRING (5, printY), v$
            COLOR _RGB32(0, 0, 0)
        NEXT i
    END IF

    IF LEN(Filter$) AND LEN(FilteredList$) = 0 THEN 'A filter is on, but nothing was found
        _PRINTSTRING (5, 4 * _FONTHEIGHT), "Search terms not found."
        _PRINTSTRING (5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to reset filter)"
    END IF

    'Top bar:
    '  SOURCE VIEW: <F5 = Run> <Trace ???> <F6 = View Variables> <F8 = Step> <F9 = Toggle Breakpoint> <ESC = Exit>
    '  Breakpoints 0 * Next line: ####
    '  Filter (code):
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), 50), _RGB32(179, 255, 255), BF
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    ModeTitle$ = "SOURCE VIEW: "
    _PRINTSTRING (5, 3), ModeTitle$
    COLOR _RGB32(255, 255, 255)
    _PRINTSTRING (4, 2), ModeTitle$
    COLOR _RGB32(0, 0, 0)
    TopLine$ = "Breakpoints: " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(TOTALBREAKPOINTS)))) + TRIM$(STR$(TOTALBREAKPOINTS)) + TAB(5) + "Next line: " + SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(CLIENT.LINENUMBER)))) + TRIM$(STR$(CLIENT.LINENUMBER)) + " (in " + TRIM$(CLIENT.CURRENTMODULE) + ")"
    _PRINTSTRING (5, (_FONTHEIGHT + 3)), TopLine$
    TopLine$ = "Filter (" + IIFSTR$(SearchIn = CODE, "code", "line") + "): " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), TopLine$

    'Top buttons:
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F5 = Run>": b = b + 1
    IF CLIENT.TOTALVARIABLES > 0 THEN
        Buttons(b).ID = 2: Buttons(b).CAPTION = "<F6 = Variables>": b = b + 1
    END IF
    Buttons(b).ID = 3: Buttons(b).CAPTION = "<Trace " + IIFSTR$(TRACE, "ON>", "OFF>"): b = b + 1
    Buttons(b).ID = 4: Buttons(b).CAPTION = IIFSTR$(STEPMODE, "<F8 = Step>", "<F8 = Pause>"): b = b + 1
    IF STEPMODE THEN
        IF LEN(FilteredList$) > 0 THEN
            IF (TOTALBREAKPOINTS > 0 AND shiftDown = -1) OR (TOTALBREAKPOINTS = LEN(FilteredList$) / 4) THEN
                Buttons(b).ID = 6: Buttons(b).CAPTION = "<F10 = Clear Breakpoints (all filtered)>": b = b + 1
            ELSE
                Buttons(b).ID = 5: Buttons(b).CAPTION = "<F9 = Set Breakpoint (all filtered)>": b = b + 1
            END IF
        ELSE
            IF TOTALBREAKPOINTS > 0 AND shiftDown = -1 THEN
                Buttons(b).ID = 6: Buttons(b).CAPTION = "<F10 = Clear Breakpoints>": b = b + 1
            ELSE
                Buttons(b).ID = 5: Buttons(b).CAPTION = "<F9 = Toggle Breakpoint>": b = b + 1
            END IF
        END IF
    ELSE
        IF LEN(FilteredList$) > 0 THEN
            IF (TOTALBREAKPOINTS > 0 AND shiftDown = -1) OR (TOTALBREAKPOINTS = LEN(FilteredList$) / 4) THEN
                Buttons(b).ID = 6: Buttons(b).CAPTION = "<F10 = Clear Breakpoints (all filtered)>": b = b + 1
            ELSE
                Buttons(b).ID = 5: Buttons(b).CAPTION = "<F9 = Set Breakpoint (all filtered)>": b = b + 1
            END IF
        ELSE
            IF TOTALBREAKPOINTS > 0 THEN
                Buttons(b).ID = 6: Buttons(b).CAPTION = "<F10 = Clear Breakpoints>": b = b + 1
            END IF
        END IF
    END IF
    Buttons(b).ID = 7: Buttons(b).CAPTION = IIFSTR$(LEN(Filter$) > 0, "<ESC = Clear filter>", "<ESC = Exit>"): b = b + 1

    IF b <= TotalButtons THEN
        Buttons(b).CAPTION = ""
    END IF

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

    IF PAGE_HEIGHT > LIST_AREA THEN
        IF LEN(Filter$) AND LEN(FilteredList$) > 0 THEN
            _PRINTSTRING (5, ((5 + (LEN(FilteredList$) / 4)) * _FONTHEIGHT) - y), ListEnd_Label + "(filtered)"
        ELSEIF LEN(Filter$) = 0 THEN
            _PRINTSTRING (5, ((5 + CLIENT.TOTALSOURCELINES) * _FONTHEIGHT) - y), ListEnd_Label
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

    ColorizeList:
    'Colorize the line if it's the next to be run...
    IF CLIENT.LINENUMBER = i THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 200, 200), BF
    END IF
    '...and if a breakpoint is set
    IF ASC(BREAKPOINTLIST, i) = 1 THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(200, 0, 0, 200), BF
        COLOR _RGB32(255, 255, 255)
    END IF
    RETURN

    DetectClick:
    'Select/Clear the item if a mouse click was detected.
    IF mb THEN
        'Wait until a mouse up event is received:
        WHILE _MOUSEBUTTON(1): _LIMIT 500: SEND_PING: mb = _MOUSEINPUT: my = _MOUSEY: mx = _MOUSEX: WEND
        mb = 0

        IF LEN(STRIPCOMMENTS$(SourceLine)) = 0 THEN
        ELSEIF STEPMODE = 0 THEN
            GOTO StepButton_Click
        ELSE
            IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN
                'Toggle breakpoint:
                IF ASC(BREAKPOINTLIST, i) = 1 THEN
                    ASC(BREAKPOINTLIST, i) = 0
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS - 1
                ELSE
                    ASC(BREAKPOINTLIST, i) = 1
                    TOTALBREAKPOINTS = TOTALBREAKPOINTS + 1
                END IF
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
                    CASE ELSE: BEEP
                END SELECT
            END IF
        NEXT cb
    END IF
    RETURN
END SUB

SUB VARIABLE_VIEW
    DIM SB_Ratio AS SINGLE
    DIM SourceLine AS STRING
    DIM ListEnd_Label AS STRING
    STATIC Filter$
    STATIC SearchIn

    TotalButtons = 6
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS , _RGB32(255, 255, 255)

    IF SearchIn = 0 THEN SearchIn = VARIABLENAMES
    SB_ThumbY = 0
    grabbedY = -1
    ListEnd_Label = "(end of list)"
    _KEYCLEAR
    longestVarName = 1

    FOR i = 1 TO CLIENT.TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > longestVarName THEN longestVarName = LEN(TRIM$(VARIABLES(i).NAME))
    NEXT i

    SWITCH_VIEW = 0

    DO: _LIMIT 500
        GOSUB ProcessInput

        SEND_PING

        GET #FILE, CLIENTBLOCK, CLIENT
        GET #FILE, DATABLOCK, VARIABLES()

        IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN STEPMODE = -1
        GOSUB UpdateList

        IF _EXIT THEN USERQUIT = -1
    LOOP UNTIL USERQUIT OR CLOSE_SESSION OR SWITCH_VIEW

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
    LOOP WHILE _MOUSEINPUT

    SELECT EVERYCASE k
        CASE 86, 118 'V
            IF ctrlDown = -1 THEN
                IF LEN(_CLIPBOARD$) THEN Filter$ = Filter$ + _CLIPBOARD$
                k = 0
            END IF
        CASE 32 TO 126 'Printable ASCII characters
            IF SearchIn <> SCOPE THEN
                Filter$ = Filter$ + CHR$(k)
            ELSE
                IF k = ASC("L") OR k = ASC("l") THEN
                    Filter$ = "LOCAL"
                ELSEIF k = ASC("S") OR k = ASC("s") THEN
                    Filter$ = "SHARED"
                END IF
            END IF
        CASE 8 'Backspace
            IF SearchIn <> SCOPE THEN
                IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
            ELSE
                Filter$ = ""
            END IF
        CASE 9, 25 'TAB alternates between what is filtered (VARIABLENAMES, DATATYPES)
            IF SearchIn = SCOPE THEN Filter$ = ""
            SELECT CASE SearchIn
                CASE SCOPE: IF shiftDown = 0 THEN SearchIn = DATATYPES ELSE SearchIn = VALUES
                CASE DATATYPES: IF shiftDown = 0 THEN SearchIn = VARIABLENAMES ELSE SearchIn = SCOPE
                CASE VARIABLENAMES: IF shiftDown = 0 THEN SearchIn = VALUES ELSE SearchIn = DATATYPES
                CASE VALUES: IF shiftDown = 0 THEN SearchIn = SCOPE ELSE SearchIn = VARIABLENAMES
            END SELECT
            IF SearchIn = SCOPE THEN Filter$ = ""
        CASE 27 'ESC clears the current search filter or exits MONITOR_MODE
            ExitButton_Click:
            IF LEN(Filter$) THEN
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
            STEPMODE = 0
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
            STEPMODE = -1
            BREAKPOINT.ACTION = NEXTSTEP
            PUT #FILE, BREAKPOINTBLOCK, BREAKPOINT
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
                CASE VALUES: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).VALUE), UCASE$(Filter$))
                CASE SCOPE: Found = MULTI_SEARCH(UCASE$(VARIABLES(i).SCOPE), UCASE$(Filter$))
            END SELECT
            IF Found THEN
                FilteredList$ = FilteredList$ + MKL$(i)
            END IF
        LOOP
        IF LEN(FilteredList$) > 0 THEN PAGE_HEIGHT = _FONTHEIGHT * ((LEN(FilteredList$) / 4) + 3)
    END IF

    'Get mouse coordinates:
    DO
    LOOP WHILE _MOUSEINPUT
    mx = _MOUSEX: my = _MOUSEY: mb = _MOUSEBUTTON(1)

    CHECK_SCREEN_LIMITS y

    'Place a light gray rectangle under the column that can currently be filtered
    SELECT CASE SearchIn
        CASE DATATYPES
            columnHighlightX = _PRINTWIDTH(SPACE$(7))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(21)) + _PRINTWIDTH(SPACE$(7))
            columnHighlightW = _PRINTWIDTH(SPACE$(longestVarName)) + 8
        CASE VALUES
            columnHighlightX = _PRINTWIDTH(SPACE$(longestVarName)) + _PRINTWIDTH(SPACE$(20)) + _PRINTWIDTH(SPACE$(7)) + 16
            columnHighlightW = _WIDTH
        CASE SCOPE
            columnHighlightX = 0
            columnHighlightW = _PRINTWIDTH(SPACE$(7))
    END SELECT
    columnHighlightY = 51

    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        columnHightlighH = (LEN(FilteredList$) / 4 * _FONTHEIGHT) + 8
    ELSEIF LEN(Filter$) > 0 AND LEN(FilteredList$) = 0 THEN
        columnHightlighH = _FONTHEIGHT
    ELSE
        columnHightlighH = (CLIENT.TOTALVARIABLES * _FONTHEIGHT) + 8
    END IF
    CLS , _RGB32(255, 255, 255)
    LINE (columnHighlightX, columnHighlightY)-STEP(columnHighlightW, columnHightlighH), _RGB32(230, 230, 230), BF

    'Print list items to the screen:
    SourceLine = TRIM$(GETLINE$(CLIENT.LINENUMBER))
    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        FOR ii = ((y \ _FONTHEIGHT) + 1) TO LEN(FilteredList$) / 4
            i = CVL(MID$(FilteredList$, ii * 4 - 3, 4))
            v$ = VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + TRIM$(VARIABLES(i).VALUE)
            printY = ((3 + ii) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO CLIENT.TOTALVARIABLES
            v$ = VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + TRIM$(VARIABLES(i).VALUE)
            printY = ((3 + i) * _FONTHEIGHT) - y
            GOSUB ColorizeSelection
            IF printY < SCREEN_HEIGHT THEN _PRINTSTRING (5, printY), v$ ELSE EXIT FOR
        NEXT i
    END IF

    IF LEN(Filter$) AND LEN(FilteredList$) = 0 THEN 'A filter is on, but nothing was found
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT), "Not found."
        _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to clear)"
    END IF

    'Top bar:
    '  VARIABLE VIEW: <F5 = Run> <F6 = View Source> <F8 = Step> <F9 = Toggle Breakpoint> <ESC = Exit>
    '  Next line: ####
    '  Filter:                                                              Total variables: 10 (showing 7)
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), 50), _RGB32(102, 255, 102), BF
    LINE (0, 0)-STEP(_WIDTH(MAINSCREEN), _FONTHEIGHT + 1), _RGB32(0, 178, 179), BF
    ModeTitle$ = "VARIABLE VIEW: "
    _PRINTSTRING (5, 3), ModeTitle$
    COLOR _RGB32(255, 255, 255)
    _PRINTSTRING (4, 2), ModeTitle$
    COLOR _RGB32(0, 0, 0)
    TopLine$ = TRIM$(CLIENT.CURRENTMODULE)
    _PRINTSTRING (_WIDTH - 3 - _PRINTWIDTH(TopLine$), 3), TopLine$
    TopLine$ = "Total variables:" + STR$(CLIENT.TOTALVARIABLES) + IIFSTR$(LEN(FilteredList$), " (showing " + TRIM$(STR$(LEN(FilteredList$) / 4)) + ")", "")
    _PRINTSTRING (_WIDTH - 5 - _PRINTWIDTH(TopLine$), (_FONTHEIGHT * 2 + 3)), TopLine$
    TopLine$ = "Next line: "
    _PRINTSTRING (3, _FONTHEIGHT + 3), TopLine$
    tl.x = 3 + _PRINTWIDTH(TopLine$)
    IF ASC(BREAKPOINTLIST, CLIENT.LINENUMBER) = 1 THEN
        LINE (tl.x, _FONTHEIGHT + 3)-STEP(_WIDTH, _FONTHEIGHT), _RGBA32(200, 0, 0, 200), BF
        COLOR _RGB32(255, 255, 255)
    END IF
    TopLine$ = SPACE$(LEN(TRIM$(STR$(CLIENT.TOTALSOURCELINES))) - LEN(TRIM$(STR$(CLIENT.LINENUMBER)))) + TRIM$(STR$(CLIENT.LINENUMBER)) + " " + CHR$(16) + " " + SourceLine
    _PRINTSTRING (tl.x, _FONTHEIGHT + 3), TopLine$
    COLOR _RGB32(0, 0, 0)
    TopLine$ = "Filter: " + UCASE$(Filter$) + IIFSTR$(cursorBlink% > 25, CHR$(179), "")
    _PRINTSTRING (5, (_FONTHEIGHT * 2 + 3)), TopLine$

    'Top buttons:
    b = 1
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F5 = Run>": b = b + 1
    Buttons(b).ID = 2: Buttons(b).CAPTION = "<F6 = Source>": b = b + 1
    Buttons(b).ID = 3: Buttons(b).CAPTION = IIFSTR$(STEPMODE, "<F8 = Step>", "<F8 = Pause>"): b = b + 1
    IF STEPMODE THEN
        Buttons(b).ID = 4: Buttons(b).CAPTION = "<F9 = Toggle Breakpoint>": b = b + 1
        Buttons(b).ID = 5: IF TOTALBREAKPOINTS > 0 AND shiftDown = -1 THEN Buttons(b).CAPTION = "<F10 = Clear Breakpoints>": b = b + 1
    ELSE
        Buttons(b).ID = 4: Buttons(b).CAPTION = "": b = b + 1
    END IF
    Buttons(b).ID = 6: Buttons(b).CAPTION = IIFSTR$(LEN(Filter$) > 0, "<ESC = Clear filter>", "<ESC = Exit>"): b = b + 1

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

    _DISPLAY
    RETURN

    ColorizeSelection:
    'Indicate that this variable is used in the current source line
    vs$ = TRIM$(VARIABLES(i).NAME)
    IF INSTR(vs$, "(") THEN vs$ = LEFT$(vs$, INSTR(vs$, "(") - 1)
    IF FIND_KEYWORD(SourceLine, vs$, FoundAt) THEN
        LINE (0, printY - 1)-STEP(_WIDTH, _FONTHEIGHT + 1), _RGBA32(200, 200, 0, 100), BF
    END IF
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
                    CASE ELSE: BEEP
                END SELECT
            END IF
        NEXT cb
    END IF
    RETURN

END SUB

SUB INTERACTIVE_MODE (VARIABLES() AS VARIABLESTYPE, AddedList$, TotalSelected)
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
    FOR i = 1 TO TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > longestVarName THEN longestVarName = LEN(TRIM$(VARIABLES(i).NAME))
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

    _AUTODISPLAY
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
            IF searchIn <> SCOPE THEN
                Filter$ = Filter$ + CHR$(k)
            ELSE
                IF k = ASC("L") OR k = ASC("l") THEN
                    Filter$ = "LOCAL"
                ELSEIF k = ASC("S") OR k = ASC("s") THEN
                    Filter$ = "SHARED"
                END IF
            END IF
        CASE 8 'Backspace
            IF searchIn <> SCOPE THEN
                IF LEN(Filter$) THEN Filter$ = LEFT$(Filter$, LEN(Filter$) - 1)
            ELSE
                Filter$ = ""
            END IF
        CASE 9, 25 'TAB alternates between what is filtered (VARIABLENAMES, DATATYPES)
            IF searchIn = SCOPE THEN Filter$ = ""
            SELECT CASE searchIn
                CASE VARIABLENAMES: IF shiftDown = 0 THEN searchIn = SCOPE ELSE searchIn = DATATYPES
                CASE SCOPE: IF shiftDown = 0 THEN searchIn = DATATYPES ELSE searchIn = VARIABLENAMES
                CASE DATATYPES: IF shiftDown = 0 THEN searchIn = VARIABLENAMES ELSE searchIn = SCOPE
            END SELECT
            IF searchIn = SCOPE THEN Filter$ = ""
        CASE 27 'ESC clears the current search filter or exits interactive mode
            CancelButton_Click:
            IF LEN(Filter$) THEN
                Filter$ = ""
            ELSE
                AddedList$ = CHR$(3)
                _AUTODISPLAY
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
            columnHighlightX = _PRINTWIDTH(SPACE$(13))
            columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
        CASE VARIABLENAMES
            columnHighlightX = _PRINTWIDTH(SPACE$(27)) + _PRINTWIDTH(SPACE$(7))
            columnHighlightW = _PRINTWIDTH(SPACE$(longestVarName)) + 8
        CASE SCOPE
            columnHighlightX = _PRINTWIDTH(SPACE$(6))
            columnHighlightW = _PRINTWIDTH(SPACE$(7))
    END SELECT
    columnHighlightY = 51

    IF LEN(Filter$) > 0 AND LEN(FilteredList$) > 0 THEN
        columnHightlighH = (LEN(FilteredList$) / 4 * _FONTHEIGHT) + 8
    ELSEIF LEN(Filter$) > 0 AND LEN(FilteredList$) = 0 THEN
        columnHightlighH = _FONTHEIGHT
    ELSE
        columnHightlighH = (TOTALVARIABLES * _FONTHEIGHT) + 8
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
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName)
            _PRINTSTRING (5, printY), v$
        NEXT ii
    ELSEIF LEN(Filter$) = 0 THEN
        FOR i = ((y \ _FONTHEIGHT) + 1) TO TOTALVARIABLES
            printY = ((3 + i) * _FONTHEIGHT) - y
            IF printY > SCREEN_HEIGHT THEN EXIT FOR
            GOSUB ColorizeSelection
            IF (my > 51) AND (my >= printY) AND (my <= (printY + _FONTHEIGHT - 1)) AND (mx < (_WIDTH - 30)) THEN GOSUB DetectClick
            v$ = "[" + IIFSTR$(ASC(AddedList$, i) = 1, "+", " ") + "]" + SPACE$(3) + VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName)
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
    Buttons(b).ID = 1: Buttons(b).CAPTION = "<F2 = Select" + IIFSTR$(LEN(Filter$), " all filtered>", " all>"): b = b + 1
    IF TotalSelected > 0 THEN
        Buttons(b).ID = 2: Buttons(b).CAPTION = "<F3 = Clear" + IIFSTR$(LEN(Filter$), " all filtered>", " all>"): b = b + 1
    END IF
    Buttons(b).ID = 3: Buttons(b).CAPTION = IIFSTR$(TotalSelected > 0, "<F5 = Save and Continue>", "<F5 = Continue>"): b = b + 1
    Buttons(b).ID = 4: Buttons(b).CAPTION = IIFSTR$(LEN(Filter$) > 0, "<ESC = Clear filter>", "<ESC = Exit>"): b = b + 1

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
                    CASE ELSE: BEEP
                END SELECT
            END IF
        NEXT cb
    END IF
    RETURN
END SUB

'------------------------------------------------------------------------------
SUB PROCESSFILE
    'Parses a .BAS file and reads all compatible variables
    'in order to generate a compatible vWATCH64 client.

    DIM InputFile AS INTEGER
    DIM OutputFile AS INTEGER
    DIM BIFile AS INTEGER
    DIM BIFileName AS STRING
    DIM BMFile AS INTEGER
    DIM BMFileName AS STRING
    DIM LOGFileName AS STRING
    DIM SourceLine AS STRING
    DIM caseBkpSourceLine AS STRING
    DIM TotalLocalVariables AS INTEGER
    DIM TotalKeywords AS INTEGER
    DIM TotalUDTs AS INTEGER
    DIM TotalUDTsAdded AS INTEGER
    DIM TotalLines AS LONG
    DIM ThisKeyword AS STRING
    DIM DefiningType AS _BIT
    DIM InBetweenSubs AS _BIT
    DIM DeclaringLibrary AS _BIT
    DIM FoundType AS STRING
    DIM MainModule AS _BYTE
    DIM LocalVariable AS _BIT
    DIM IsArray AS _BIT
    DIM bkpSourceLine$
    DIM NextVar$
    DIM caseBkpNextVar$
    DIM DefaultTypeUsed AS _BIT
    DIM CHECKSUM AS STRING * 8
    REDIM UDT(1) AS UDTTYPE, UDT_ADDED(1) AS VARIABLESTYPE
    REDIM LOCALVARIABLES(1) AS VARIABLESTYPE
    REDIM LOCALSHAREDADDED(1) AS STRING
    REDIM KeywordList(1) AS STRING

    RESTORE KeyWordsDATA
    'Populate KeywordList() with DATA TYPES:
    DO
        READ ThisKeyword
        IF UCASE$(ThisKeyword) = "END" THEN
            INTERNALKEYWORDS = TotalKeywords
            EXIT DO
        END IF
        GOSUB AddThisKeyword
    LOOP

    Q$ = CHR$(34)

    'Process dialog:
    '-----------------------------------------------------------
    'vWATCH64 - v.951b
    'Processing file: xxxxx.bas
    'New file name:
    '
    '  Include arrays?            < Yes >
    '  Launch interactive mode?   < No  >
    '  Compile?                   < No  >
    '  Show details?              < No  >
    '                                          < OK > < Cancel >
    '-----------------------------------------------------------
    DialogX = _WIDTH(MAINSCREEN) / 2 - 200
    DialogY = _HEIGHT(MAINSCREEN) / 2 - 100

    '---------------------------------------------
    IF LEN(TRIM$(NEWFILENAME$)) = 0 THEN
        ShowProcessDialog:
        Dialog = 1
        CLS , _RGB32(255, 255, 255)

        'Dialog buttons:
        TotalButtons = 6
        REDIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

        getfilename% = GLIINPUT(DialogX + 5, DialogY + 5 + _FONTHEIGHT * 2, GLIALPHA + GLINUMERIC + GLIDASH, "New file name: ", TRUE)
        DIALOGRESULT = 0
        DO
            LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF
            COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
            _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
            LINE (DialogX + 401, DialogY + 5 + _FONTHEIGHT)-STEP(_WIDTH - (DialogX + 401), _FONTHEIGHT), _RGB32(255, 255, 255), BF
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 5), "Include arrays?"
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 6), "Launch interactive mode?"
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 7), "Compile?"
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 8), "Show details?"

            GLICLEAR
            GLIUPDATE

            'Draw buttons
            b = 1
            Buttons(b).CAPTION = IIFSTR$(SKIPARRAYS, "< No  >", "< Yes >"): b = b + 1
            Buttons(b).CAPTION = IIFSTR$(INTERACTIVE, "< Yes >", "< No  >"): b = b + 1
            Buttons(b).CAPTION = IIFSTR$(DONTCOMPILE, "< No  >", "< Yes >"): b = b + 1
            Buttons(b).CAPTION = IIFSTR$(VERBOSE, "< Yes >", "< No  >"): b = b + 1
            Buttons(b).CAPTION = "< OK >": b = b + 1
            Buttons(b).CAPTION = "< Cancel >": b = b + 1

            FOR cb = 1 TO TotalButtons
                IF cb <= 4 THEN
                    Buttons(cb).Y = DialogY + 5 + _FONTHEIGHT * (cb + 4) + 1
                    Buttons(cb).X = DialogX + 200
                ELSE
                    Buttons(cb).Y = (DialogY + 200) - (_FONTHEIGHT) - 5
                END IF
                Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
            NEXT cb
            Buttons(5).X = ((DialogX + 400) - _PRINTWIDTH(TRIM$(Buttons(6).CAPTION))) - _PRINTWIDTH(TRIM$(Buttons(5).CAPTION))
            Buttons(6).X = (DialogX + 400) - _PRINTWIDTH(TRIM$(Buttons(6).CAPTION))

            GOSUB CheckButtons
            FOR cb = 1 TO TotalButtons
                _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), TRIM$(Buttons(cb).CAPTION)
            NEXT cb
            'end of drawing buttons

            _DISPLAY
            k$ = INKEY$
            IF k$ = CHR$(27) THEN DIALOGRESULT = 2
        LOOP UNTIL GLIENTERED(getfilename%) OR DIALOGRESULT > 0
        IF DIALOGRESULT = 2 THEN GLICLOSE getfilename%, TRUE: EXIT SUB
        NEWFILENAME$ = GLIOUTPUT$(getfilename%)
        GLICLOSE getfilename%, TRUE
    END IF

    'Check if processing can proceed:
    IF LEN(TRIM$(NEWFILENAME$)) = 0 THEN
        GOTO ShowProcessDialog
    END IF

    BIFileName = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4), NEWFILENAME$) + ".bi"
    BMFileName = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4), NEWFILENAME$) + ".bm"
    LOGFileName = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4), NEWFILENAME$) + ".log"
    NEWFILENAME$ = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", NEWFILENAME$, NEWFILENAME$ + ".bas")

    IF UCASE$(FILENAME$) = UCASE$(NEWFILENAME$) THEN
        BEEP
        GOTO ShowProcessDialog
    END IF

    IF _FILEEXISTS(NEWFILENAME$) THEN
        'Overwrite dialog:
        '-----------------------------------------------------------
        'vWATCH64 - v.951b
        'Processing file: xxxxx.bas
        'New file name: yyyyy.bas
        '
        '
        '  File already exists.
        '  Replace?
        '
        '                                           < Yes > <  No  >
        '-----------------------------------------------------------
        ShowOverwriteDialog:
        Dialog = 2
        CLS , _RGB32(255, 255, 255)

        'Dialog buttons:
        TotalButtons = 2
        REDIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE

        '---------------------------------------------
        OVERWRITE = 0
        DO
            LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF
            COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
            _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
            COLOR _RGB32(255, 0, 0)
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 2), "New file name: " + NOPATH$(NEWFILENAME$)
            COLOR _RGB32(0, 0, 0)
            LINE (DialogX + 401, DialogY + 5 + _FONTHEIGHT)-STEP(_WIDTH - (DialogX + 401), _FONTHEIGHT), _RGB32(255, 255, 255), BF
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 6), "File already exists."
            _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 7), "Replace?"

            'Draw buttons
            b = 1
            Buttons(b).CAPTION = "< Yes >": b = b + 1
            Buttons(b).CAPTION = "<  No  >": b = b + 1

            FOR cb = 1 TO TotalButtons
                Buttons(cb).Y = (DialogY + 200) - (_FONTHEIGHT) - 5
                Buttons(cb).W = _PRINTWIDTH(TRIM$(Buttons(cb).CAPTION))
            NEXT cb
            Buttons(1).X = ((DialogX + 400) - _PRINTWIDTH(TRIM$(Buttons(2).CAPTION))) - _PRINTWIDTH(TRIM$(Buttons(1).CAPTION))
            Buttons(2).X = (DialogX + 400) - _PRINTWIDTH(TRIM$(Buttons(2).CAPTION))

            GOSUB CheckButtons
            FOR cb = 1 TO TotalButtons
                _PRINTSTRING (Buttons(cb).X, Buttons(cb).Y), TRIM$(Buttons(cb).CAPTION)
            NEXT cb
            'end of drawing buttons
            k$ = INKEY$
            IF UCASE$(k$) = "Y" OR k$ = CHR$(13) THEN OVERWRITE = 1: _KEYCLEAR
            IF UCASE$(k$) = "N" OR k$ = CHR$(27) THEN OVERWRITE = 2: _KEYCLEAR
            _DISPLAY
        LOOP UNTIL OVERWRITE = 1 OR OVERWRITE = 2
    END IF

    'Check if processing can proceed:
    IF OVERWRITE = 2 THEN
        GOTO ShowProcessDialog
    END IF

    'Processing can proceed.
    _AUTODISPLAY
    COLOR _RGB32(0, 0, 0), _RGB32(230, 230, 230)
    CLS
    PRINT: PRINT
    InputFile = FREEFILE
    OPEN FILENAME$ FOR BINARY AS #InputFile

    'Read the source into memory:
    SOURCEFILE = SPACE$(LOF(InputFile))
    GET #InputFile, 1, SOURCEFILE

    SEEK #InputFile, 1
    CHECKSUM = ADLER32(SOURCEFILE)
    SOURCEFILE = ""

    OutputFile = FREEFILE
    OPEN NEWFILENAME$ FOR OUTPUT AS #OutputFile

    MainModule = -1
    'Inject the required code into processed file:
    PRINT #OutputFile, "'$INCLUDE:'" + BIFileName + "'"

    'Look for variables inside the main module and stores information in VARIABLES()
    'and LOCALVARIABLES. If SUB or FUNCTION is found, injects CURRENTMODULE verification
    'code. If SYSTEM is found, injects cleanup procedures (also when main module ends):
    TOTALVARIABLES = 0
    PRINT "Parsing .BAS and injecting breakpoint control code...";
    IF VERBOSE THEN PRINT
    row = CSRLIN: col = POS(1)
    TotalSourceLines = 0
    MULTILINE = 0
    DO
        k$ = INKEY$
        IF k$ = CHR$(27) THEN
            BEEP
            PRINT
            PRINT
            COLOR _RGB32(255, 0, 0)
            PRINT "Processing canceled."
            COLOR _RGB32(0, 0, 0)
            PRINT "Press any key..."
            CLOSE OutputFile
            CLOSE InputFile
            KILL NEWFILENAME$
            SLEEP
            EXIT SUB
        END IF
        IF LEN(caseBkpNextVar$) = 0 THEN 'Read next line from file unless we're in the middle of processing a line
            NextLineStart = SEEK(InputFile)
            LINE INPUT #InputFile, bkpSourceLine$ 'Read the next source line
            TotalSourceLines = TotalSourceLines + 1
            caseBkpSourceLine = TRIM$(STRIPCOMMENTS(bkpSourceLine$)) 'Generate a version without comments or extra spaces
            SourceLine = UCASE$(caseBkpSourceLine) 'Generate an all upper case version

            'BREAKPOINTS: Handle exceptions - cases in which a call to vwatch64_CHECKBREAKPOINT is
            'not necessary (comments, CONST, etc...) or not allowed (CASE, TYPE, etc...):
            IF DefiningType = 0 AND InBetweenSubs = 0 AND DeclaringLibrary = 0 THEN
                IF LEN(SourceLine) = 0 THEN
                ELSEIF LEFT$(SourceLine, 1) = "$" THEN
                ELSEIF LEFT$(SourceLine, 4) = "DIM " THEN
                ELSEIF LEFT$(SourceLine, 5) = "DATA " THEN
                ELSEIF LEFT$(SourceLine, 5) = "CASE " THEN
                ELSEIF LEFT$(SourceLine, 5) = "TYPE " THEN
                ELSEIF LEFT$(SourceLine, 6) = "REDIM " THEN
                ELSEIF LEFT$(SourceLine, 6) = "CONST " THEN
                ELSEIF LEFT$(SourceLine, 7) = "STATIC " THEN
                ELSE
                    PRINT #OutputFile, "vwatch64_CHECKBREAKPOINT " + TRIM$(STR$(TotalSourceLines))
                END IF
            END IF

            IF NOT VERBOSE THEN LOCATE row, col: PRINT USING "###"; (SEEK(InputFile) / LOF(InputFile)) * 100;: PRINT "% (Watchable variables found: "; TRIM$(STR$(TOTALVARIABLES)); ")"
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
                    IF VERBOSE THEN
                        PRINT "Found UDT: "; TRIM$(UDT(TotalUDTs).UDT); "."; TRIM$(UDT(TotalUDTs).ELEMENT); " AS "; TRIM$(UDT(TotalUDTs).DATATYPE)
                        _DELAY .05
                    END IF
                END IF
            END IF
        END IF

        IF MULTILINE THEN SourceLine = IIFSTR$(LocalVariable, "DIM ", "DIM SHARED ") + SourceLine: MULTILINE = 0

        IF LEFT$(SourceLine, 4) = "DIM " AND MainModule THEN
            LocalVariable = 0
            IF MID$(SourceLine, 5, 7) <> "SHARED " THEN LocalVariable = -1

            IF LEN(caseBkpNextVar$) > 0 THEN
                NextVar$ = UCASE$(caseBkpNextVar$)
            ELSE
                caseBkpNextVar$ = GETNEXTVARIABLE$(caseBkpSourceLine)
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
                            VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                            VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                        NEXT i
                    END IF
                ELSE
                    TOTALVARIABLES = TOTALVARIABLES + 1
                    REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                    VARIABLES(TOTALVARIABLES).NAME = caseBkpNextVar$
                    VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                    VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                END IF

                IF LocalVariable THEN
                    TotalLocalVariables = TotalLocalVariables + 1
                    REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                    LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                    LOCALVARIABLES(TotalLocalVariables).DATATYPE = IIFSTR$(DefaultTypeUsed, "", VARIABLES(TOTALVARIABLES).DATATYPE)
                END IF

                IF VERBOSE THEN
                    PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                    PRINT VARIABLES(TOTALVARIABLES).DATATYPE,
                    PRINT TRIM$(VARIABLES(TOTALVARIABLES).NAME)
                    _DELAY .05
                END IF
            ELSE
                FoundType = RIGHT$(NextVar$, LEN(NextVar$) - INSTR(NextVar$, " AS ") - 3)

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
                                VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                                VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                            NEXT i
                        END IF
                    ELSE
                        TOTALVARIABLES = TOTALVARIABLES + 1
                        REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                        VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1)
                        VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                        VARIABLES(TOTALVARIABLES).DATATYPE = FoundType
                    END IF

                    IF LocalVariable THEN
                        TotalLocalVariables = TotalLocalVariables + 1
                        REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                        LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                        LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                    END IF

                    IF VERBOSE THEN
                        PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                        PRINT VARIABLES(TOTALVARIABLES).DATATYPE;
                        PRINT TRIM$(VARIABLES(TOTALVARIABLES).NAME)
                        _DELAY .05
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
                                        VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                                        VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                        VARIABLES(TOTALVARIABLES).DATATYPE = TRIM$(UDT(i).DATATYPE)

                                        IF LocalVariable THEN
                                            TotalLocalVariables = TotalLocalVariables + 1
                                            REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                            LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                            LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                        END IF

                                        IF VERBOSE THEN
                                            PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                                            PRINT UDT(i).DATATYPE;
                                            PRINT TRIM$(VARIABLES(TOTALVARIABLES).NAME)
                                            _DELAY .05
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
                                VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                                VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                VARIABLES(TOTALVARIABLES).DATATYPE = TRIM$(UDT(i).DATATYPE)

                                IF LocalVariable THEN
                                    TotalLocalVariables = TotalLocalVariables + 1
                                    REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                    LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                    LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                END IF

                                IF VERBOSE THEN
                                    PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                                    PRINT UDT(i).DATATYPE;
                                    PRINT TRIM$(VARIABLES(TOTALVARIABLES).NAME)
                                    _DELAY .05
                                END IF
                            END IF
                        NEXT i
                    END IF
                END IF
            END IF
            caseBkpNextVar$ = GETNEXTVARIABLE$(caseBkpSourceLine)
            IF LEN(caseBkpNextVar$) = 0 THEN
                PRINT #OutputFile, bkpSourceLine$
                IF RIGHT$(SourceLine, 1) = "_" THEN MULTILINE = -1
            END IF
        ELSEIF LEFT$(SourceLine, 8) = "DECLARE " THEN
            IF INSTR(SourceLine, " LIBRARY") THEN DeclaringLibrary = -1
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 11) = "END DECLARE" THEN
            DeclaringLibrary = 0
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 13) = "OPTION BASE 1" THEN
            SET_OPTIONBASE = 1
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFINT " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "INTEGER"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFLNG " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "LONG"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSTR " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "STRING"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSNG " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "SINGLE"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFDBL " THEN
            SET_DEF RIGHT$(SourceLine, LEN(SourceLine) - 7), "DOUBLE"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 8) = "_DEFINE " THEN
            IF INSTR(SourceLine, " AS ") > 0 THEN
                SET_DEF MID$(SourceLine, 9, INSTR(SourceLine, " AS ") - 9), RIGHT$(SourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3)
            END IF
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 5) = "TYPE " THEN
            'User defined types will be added to the DATA TYPE keyword list:
            ThisKeyword = RIGHT$(caseBkpSourceLine, LEN(SourceLine) - 5)
            GOSUB AddThisKeyword
            DefiningType = -1
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 4) = "SUB " THEN
            InBetweenSubs = 0
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    PRINT #OutputFile, "IF vwatch64_HEADER.CONNECTED THEN"
                    PRINT #OutputFile, "    vwatch64_HEADER.CONNECTED = 0"
                    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
                    PRINT #OutputFile, "    CLOSE #vwatch64_CLIENTFILE"
                    PRINT #OutputFile, "END IF"
                    PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
                    PRINT #OutputFile, "KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
                    PRINT #OutputFile, ""
                    PRINT #OutputFile, "SYSTEM"
                    PRINT #OutputFile, "vwatch64_FILEERROR:"
                    PRINT #OutputFile, "RESUME NEXT"
                    PRINT #OutputFile, ""
                END IF
                PRINT #OutputFile, bkpSourceLine$
                IF INSTR(SourceLine, "(") THEN
                    IF VERBOSE THEN PRINT "Found: SUB "; MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5)
                    SourceLine = "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "SUB " + MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5) + Q$
                ELSE
                    IF VERBOSE THEN PRINT "Found: "; caseBkpSourceLine
                    SourceLine = "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + caseBkpSourceLine + Q$
                END IF
                PRINT #OutputFile, SourceLine
                IF VERBOSE THEN _DELAY .05
            ELSE
                PRINT #OutputFile, bkpSourceLine$
            END IF
        ELSEIF LEFT$(SourceLine, 9) = "FUNCTION " THEN
            InBetweenSubs = 0
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    PRINT #OutputFile, "IF vwatch64_HEADER.CONNECTED THEN"
                    PRINT #OutputFile, "    vwatch64_HEADER.CONNECTED = 0"
                    PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
                    PRINT #OutputFile, "    CLOSE #vwatch64_CLIENTFILE"
                    PRINT #OutputFile, "END IF"
                    PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
                    PRINT #OutputFile, "KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
                    PRINT #OutputFile, ""
                    PRINT #OutputFile, "SYSTEM"
                    PRINT #OutputFile, "vwatch64_FILEERROR:"
                    PRINT #OutputFile, "RESUME NEXT"
                    PRINT #OutputFile, ""
                END IF
                PRINT #OutputFile, bkpSourceLine$
                IF INSTR(SourceLine, "(") THEN
                    IF VERBOSE THEN PRINT "Found: FUNCTION "; MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10)
                    SourceLine = "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "FUNCTION " + MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10) + Q$
                ELSE
                    IF VERBOSE THEN PRINT "Found: FUNCTION "; caseBkpSourceLine
                    SourceLine = "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + caseBkpSourceLine + Q$
                END IF
                PRINT #OutputFile, SourceLine
                IF VERBOSE THEN _DELAY .05
            ELSE
                IF SourceLine <> UCASE$("FUNCTION GetModuleFileNameA (BYVAL hModule AS LONG, lpFileName AS STRING, BYVAL nSize AS LONG)") THEN
                    PRINT #OutputFile, bkpSourceLine$
                ELSE
                    PRINT #OutputFile, "'" + bkpSourceLine$
                    PRINT #OutputFile, "'FUNCTION declaration skipped because it's already in the $INCLUDEd file (line 1)."
                END IF
            END IF
        ELSEIF LEFT$(SourceLine, 7) = "END SUB" OR LEFT$(SourceLine, 12) = "END FUNCTION" THEN
            PRINT #OutputFile, "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
            PRINT #OutputFile, bkpSourceLine$
            InBetweenSubs = -1
        ELSEIF INSTR(SourceLine, "EXIT SUB") OR INSTR(SourceLine, "EXIT FUNCTION") THEN
            IF LEFT$(SourceLine, 5) <> "CASE " THEN
                PRINT #OutputFile, "vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
            END IF
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF SourceLine = "SYSTEM" OR SourceLine = "END" THEN
            PRINT #OutputFile, "IF vwatch64_HEADER.CONNECTED THEN"
            PRINT #OutputFile, "    vwatch64_HEADER.CONNECTED = 0"
            PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
            PRINT #OutputFile, "    CLOSE #vwatch64_CLIENTFILE"
            PRINT #OutputFile, "END IF"
            PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
            PRINT #OutputFile, "KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
            PRINT #OutputFile, ""
            PRINT #OutputFile, bkpSourceLine$
        ELSE
            PRINT #OutputFile, bkpSourceLine$
        END IF
    LOOP UNTIL EOF(InputFile)

    IF TOTALVARIABLES = 0 THEN
        PRINT
        PRINT
        COLOR _RGB32(255, 0, 0)
        PRINT "There are no watchable variables in the .BAS source."
        COLOR _RGB32(0, 0, 0)
        PRINT "(watchable variables are those initialized using DIM)"
        PRINT
        _DELAY .05
    ELSE
        PRINT "Total watchable variables found: "; TOTALVARIABLES
        IF VERBOSE THEN _DELAY .05
        IF INTERACTIVE THEN
            bkpx% = POS(1): bkpy% = CSRLIN
            BackupScreen = _COPYIMAGE(0)
            INTERACTIVE_MODE VARIABLES(), AddedList$, TotalSelected
            CLS , _RGB32(230, 230, 230)
            _PUTIMAGE (0, 0), BackupScreen
            _FREEIMAGE BackupScreen
            LOCATE bkpy%, bkpx%
            IF AddedList$ = CHR$(3) THEN
                'Processing was canceled by user.
                PRINT
                COLOR _RGB32(255, 0, 0)
                PRINT "Processing canceled."
                COLOR _RGB32(0, 0, 0)
                PRINT
                CLOSE OutputFile
                CLOSE InputFile
                KILL NEWFILENAME$
                _DELAY 1
                EXIT SUB
            ELSE
                PRINT IIFSTR$(TOTALVARIABLES = TotalSelected, "All", STR$(TotalSelected)); " variable"; IIFSTR$(TotalSelected > 1, "s", ""); " selected."
            END IF
        ELSE
            AddedList$ = STRING$(TOTALVARIABLES, 1)
            TotalSelected = TOTALVARIABLES
        END IF
    END IF

    IF MainModule THEN 'All lines have been parsed. This .BAS contains no SUBs/FUNCTIONs.
        MainModule = 0
        PRINT #OutputFile, "IF vwatch64_HEADER.CONNECTED THEN"
        PRINT #OutputFile, "    vwatch64_HEADER.CONNECTED = 0"
        PRINT #OutputFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
        PRINT #OutputFile, "    CLOSE #vwatch64_CLIENTFILE"
        PRINT #OutputFile, "END IF"
        PRINT #OutputFile, "ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
        PRINT #OutputFile, ""
        PRINT #OutputFile, "SYSTEM"
        PRINT #OutputFile, "vwatch64_FILEERROR:"
        PRINT #OutputFile, "RESUME NEXT"
        PRINT #OutputFile, ""
    END IF
    PRINT #OutputFile,
    PRINT #OutputFile, "'$INCLUDE:'" + BMFileName + "'"
    CLOSE OutputFile
    CLOSE InputFile

    BIFile = FREEFILE
    OPEN BIFileName FOR OUTPUT AS #BIFile

    PRINT "Generating "; BIFileName; "..."
    'Creates a vWATCH64.BI customized for the .BAS provided:
    $IF WIN THEN
        PRINT #BIFile, "        DECLARE LIBRARY"
        PRINT #BIFile, "            FUNCTION GetModuleFileNameA (BYVAL hModule AS LONG, lpFileName AS STRING, BYVAL nSize AS LONG)"
        PRINT #BIFile, "        END DECLARE"
    $END IF
    PRINT #BIFile, ""
    PRINT #BIFile, "    DECLARE LIBRARY " + Q$ + "timers" + Q$
    PRINT #BIFile, "        SUB VWATCH64_STOPTIMERS ALIAS stop_timers"
    PRINT #BIFile, "        SUB VWATCH64_STARTTIMERS ALIAS start_timers"
    PRINT #BIFile, "    END DECLARE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    CONST vwatch64_ID = " + Q$ + "vWATCH64" + Q$
    PRINT #BIFile, "    CONST vwatch64_VERSION = " + Q$ + VERSION + Q$
    PRINT #BIFile, "    CONST vwatch64_INTERVAL = .1"
    PRINT #BIFile, "    CONST vwatch64_CHECKSUM = " + Q$ + CHECKSUM + Q$
    PRINT #BIFile, "    CONST vwatch64_TIMEOUTLIMIT =" + STR$(TIMEOUTLIMIT)
    PRINT #BIFile, ""
    PRINT #BIFile, "    'Breakpoint control:"
    PRINT #BIFile, "    CONST vwatch64_CONTINUE = 1"
    PRINT #BIFile, "    CONST vwatch64_NEXTSTEP = 2"
    PRINT #BIFile, "    CONST vwatch64_READY = 3"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_HEADERTYPE"
    PRINT #BIFile, "        CLIENT_ID AS STRING * 8"
    PRINT #BIFile, "        VERSION AS STRING * 5"
    PRINT #BIFile, "        CONNECTED AS _BYTE"
    PRINT #BIFile, "        RESPONSE AS _BYTE"
    PRINT #BIFile, "        HOST_PING AS _BYTE"
    PRINT #BIFile, "        CLIENT_PING AS _BYTE"
    PRINT #BIFile, "        HISTORY_LOG AS _BYTE"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_CLIENTTYPE"
    PRINT #BIFile, "        NAME AS STRING * 256"
    PRINT #BIFile, "        CHECKSUM AS STRING * 8"
    PRINT #BIFile, "        TOTALSOURCELINES AS LONG"
    PRINT #BIFile, "        EXENAME AS STRING * 256"
    PRINT #BIFile, "        CURRENTMODULE AS STRING * 50"
    PRINT #BIFile, "        LINENUMBER AS LONG"
    PRINT #BIFile, "        TOTALVARIABLES AS LONG"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_BREAKPOINTTYPE"
    PRINT #BIFile, "        ACTION AS _BYTE"
    PRINT #BIFile, "    END TYPE"
    IF TotalSelected > 0 THEN
        PRINT #BIFile, ""
        PRINT #BIFile, "    TYPE vwatch64_VARIABLESTYPE"
        PRINT #BIFile, "        NAME AS STRING * 256"
        PRINT #BIFile, "        SCOPE AS STRING * 7"
        PRINT #BIFile, "        UDT AS STRING * 40"
        PRINT #BIFile, "        DATATYPE AS STRING * 20"
        PRINT #BIFile, "        VALUE AS STRING * 255"
        PRINT #BIFile, "    END TYPE"
    END IF
    PRINT #BIFile, ""
    PRINT #BIFile, "    DIM SHARED vwatch64_BREAKPOINT AS vwatch64_BREAKPOINTTYPE"
    PRINT #BIFile, "    DIM SHARED vwatch64_BREAKPOINTBLOCK AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_BREAKPOINTLISTBLOCK AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_BREAKPOINTLIST AS STRING *" + STR$(TotalSourceLines)
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENT AS vwatch64_CLIENTTYPE"
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENTBLOCK AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENTFILE AS INTEGER"
    PRINT #BIFile, "    DIM SHARED vwatch64_LOGFILE AS INTEGER"
    PRINT #BIFile, "    DIM SHARED vwatch64_DATABLOCK AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_HEADER AS vwatch64_HEADERTYPE"
    PRINT #BIFile, "    DIM SHARED vwatch64_HEADERBLOCK AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_LOF AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_USERQUIT AS _BIT"
    PRINT #BIFile, "    DIM SHARED vwatch64_LAST_PING#"
    PRINT #BIFile, "    DIM SHARED vwatch64_LOGOPEN AS _BIT"
    PRINT #BIFile, ""
    IF TotalSelected > 0 THEN
        PRINT #BIFile, "    DIM SHARED vwatch64_VARIABLES(1 TO " + TRIM$(STR$(TotalSelected)) + ") AS vwatch64_VARIABLESTYPE"
        PRINT #BIFile, "    DIM SHARED vwatch64_PREVVARIABLES(1 TO " + TRIM$(STR$(TotalSelected)) + ") AS STRING * 255"
        tempindex = 0
        FOR i = 1 TO TOTALVARIABLES
            IF ASC(AddedList$, i) = 1 THEN
                tempindex = tempindex + 1
                PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").NAME = " + Q$ + TRIM$(VARIABLES(i).NAME) + Q$
                PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").SCOPE = " + Q$ + TRIM$(VARIABLES(i).SCOPE) + Q$
                PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").DATATYPE = " + Q$ + TRIM$(VARIABLES(i).DATATYPE) + Q$
            END IF
        NEXT i
        PRINT #BIFile, ""
    END IF
    PRINT #BIFile, "    vwatch64_HEADERBLOCK = 1"
    PRINT #BIFile, "    vwatch64_CLIENTBLOCK = LEN(vwatch64_HEADER) + 1"
    PRINT #BIFile, "    vwatch64_BREAKPOINTBLOCK = vwatch64_CLIENTBLOCK + LEN(vwatch64_CLIENT) + 1"
    PRINT #BIFile, "    vwatch64_BREAKPOINTLISTBLOCK = vwatch64_BREAKPOINTBLOCK + LEN(vwatch64_BREAKPOINT) + 1"
    PRINT #BIFile, "    vwatch64_DATABLOCK = vwatch64_BREAKPOINTLISTBLOCK + LEN(vwatch64_BREAKPOINTLIST) + 1"
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_CONNECTTOHOST"
    PRINT #BIFile, ""
    CLOSE BIFile

    BMFile = FREEFILE
    OPEN BMFileName FOR OUTPUT AS #BMFile

    PRINT "Generating "; BMFileName; "..."
    IF VERBOSE THEN _DELAY .05
    'Creates a vWATCH64.BM customized for the .BAS provided:
    PRINT #BMFile, "SUB vwatch64_CONNECTTOHOST"
    RANDOMIZE TIMER
    PRINT #BMFile, "    DIM vwatch64_EXENAME AS STRING * 256"
    PRINT #BMFile, ""
    PRINT #BMFile, "    _TITLE " + Q$ + "Connecting to vWATCH64..." + Q$
    PRINT #BMFile, ""
    PRINT #BMFile, "    vwatch64_CLIENTFILE = " + LTRIM$(TRIM$(STR$(_CEIL(RND * 30000) + 100)))
    PRINT #BMFile, "    vwatch64_LOGFILE = " + LTRIM$(TRIM$(STR$(_CEIL(RND * 30000) + 100)))
    PRINT #BMFile, "    IF vwatch64_LOGFILE = vwatch64_CLIENTFILE THEN vwatch64_LOGFILE = vwatch64_LOGFILE + 1"
    PRINT #BMFile, "    'You may be wondering why such random file numbers..."
    PRINT #BMFile, "    OPEN " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$ + " FOR BINARY AS vwatch64_CLIENTFILE"
    PRINT #BMFile, ""
    PRINT #BMFile, "    'Check if a connection is already active"
    PRINT #BMFile, "    GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "    IF vwatch64_HEADER.CONNECTED = -1 THEN"
    PRINT #BMFile, "        'Check if the existing file belongs to this program."
    PRINT #BMFile, "        GET #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT"
    PRINT #BMFile, "        IF vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM THEN"
    PRINT #BMFile, "            EXIT SUB"
    PRINT #BMFile, "        ELSE"
    PRINT #BMFile, "            _TITLE " + Q$ + "FAILED!" + Q$
    PRINT #BMFile, "            vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "            CLOSE #vwatch64_CLIENTFILE"
    PRINT #BMFile, "            ON ERROR GOTO vwatch64_FILEERROR"
    PRINT #BMFile, "            KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
    PRINT #BMFile, "            EXIT SUB"
    PRINT #BMFile, "        END IF"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, ""
    PRINT #BMFile, "    vwatch64_CLIENT.NAME = " + Q$ + FILENAME$ + Q$
    PRINT #BMFile, "    vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM"
    PRINT #BMFile, "    vwatch64_CLIENT.TOTALSOURCELINES =" + STR$(TotalSourceLines)
    PRINT #BMFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
    PRINT #BMFile, "    vwatch64_CLIENT.TOTALVARIABLES =" + STR$(TotalSelected)
    PRINT #BMFile, ""
    $IF WIN THEN
        PRINT #BMFile, "    Ret = GetModuleFileNameA(0, vwatch64_EXENAME, LEN(vwatch64_EXENAME))"
        PRINT #BMFile, "    IF Ret > 0 THEN"
        PRINT #BMFile, "        vwatch64_CLIENT.EXENAME = LEFT$(vwatch64_EXENAME, Ret)"
        PRINT #BMFile, "    END IF"
    $ELSE
        PRINT #BMFile, "    vwatch64_CLIENT.EXENAME = " + Q$ + Q$
    $END IF
    PRINT #BMFile, ""
    PRINT #BMFile, "    'Send this client's version and connection request"
    PRINT #BMFile, "    vwatch64_HEADER.CLIENT_ID = vwatch64_ID"
    PRINT #BMFile, "    vwatch64_HEADER.VERSION = vwatch64_VERSION"
    PRINT #BMFile, "    vwatch64_HEADER.CONNECTED = -1"
    PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
    PRINT #BMFile, ""
    PRINT #BMFile, "    'Wait for authorization:"
    PRINT #BMFile, "    vwatch64_WAITSTART# = TIMER"
    PRINT #BMFile, "    DO: _LIMIT 30"
    PRINT #BMFile, "        _TITLE " + Q$ + "Connecting to vWATCH64 (F4 to cancel and start logging)... (" + Q$ + " + LTRIM$(STR$(3 - INT(TIMER - vwatch64_WAITSTART#))) + " + Q$ + ")" + Q$
    PRINT #BMFile, "        GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "        k = _KEYHIT"
    PRINT #BMFile, "        IF k = 27 THEN EXIT DO"
    PRINT #BMFile, "        IF k = 15872 THEN vwatch64_HEADER.HISTORY_LOG = -1: EXIT DO"
    PRINT #BMFile, "        IF TIMER - vwatch64_WAITSTART# > 3 THEN EXIT DO"
    PRINT #BMFile, "     LOOP UNTIL vwatch64_HEADER.RESPONSE = -1"
    PRINT #BMFile, ""
    PRINT #BMFile, "    IF vwatch64_HEADER.RESPONSE = 0 AND vwatch64_HEADER.HISTORY_LOG = 0 THEN"
    PRINT #BMFile, "        _TITLE " + Q$ + "FAILED!" + Q$
    PRINT #BMFile, "        vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "        CLOSE #vwatch64_CLIENTFILE"
    PRINT #BMFile, "        ON ERROR GOTO vwatch64_FILEERROR"
    PRINT #BMFile, "        KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
    PRINT #BMFile, "        EXIT SUB"
    PRINT #BMFile, "    ELSEIF vwatch64_HEADER.RESPONSE = 0 AND vwatch64_HEADER.HISTORY_LOG = -1 THEN"
    PRINT #BMFile, "        _TITLE " + Q$ + "LOGGING STARTED!" + Q$
    PRINT #BMFile, "        OPEN " + Q$ + PATHONLY$(EXENAME) + NOPATH$(LOGFileName) + Q$ + " FOR APPEND AS vwatch64_LOGFILE"
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, STRING$(80, 45)"
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, " + Q$ + "vWATCH64 v" + Q$ + "; vwatch64_VERSION"
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, " + Q$ + "Logging: " + FILENAME$ + Q$
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, " + Q$ + "Started: " + Q$ + "; DATE$, TIME$"
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, STRING$(80, 45)"
    PRINT #BMFile, "        vwatch64_LOGOPEN = -1"
    PRINT #BMFile, "    ELSE"
    PRINT #BMFile, "        PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT"
    PRINT #BMFile, "        vwatch64_LAST_PING# = TIMER"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, "END SUB"
    PRINT #BMFile, ""
    IF TotalSelected > 0 THEN
        PRINT #BMFile, "SUB vwatch64_VARIABLEWATCH"
        LocalSharedAddedTotal = 0
        FOR i = 1 TO TotalLocalVariables
            SourceLine = "    SHARED "
            Found = FINDVARIABLES(TRIM$(LOCALVARIABLES(i).NAME), AddedList$)
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
            PRINT #BMFile, LocalShared_NOREPETITION(i)
        NEXT i

        tempindex = 0
        FOR i = 1 TO TOTALVARIABLES
            IF ASC(AddedList$, i) = 1 THEN
                tempindex = tempindex + 1
                IF INSTR(VARIABLES(i).DATATYPE, "STRING") THEN
                    SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").VALUE = " + TRIM$(VARIABLES(i).NAME)
                    PRINT #BMFile, SourceLine
                ELSE
                    SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(tempindex)) + ").VALUE = STR$(" + TRIM$(VARIABLES(i).NAME) + ")"
                    PRINT #BMFile, SourceLine
                END IF
            END IF
        NEXT i
        PRINT #BMFile, "    IF vwatch64_HEADER.HISTORY_LOG = -1 THEN"
        PRINT #BMFile, "        FOR i = 1 to " + LTRIM$(STR$(TotalSelected))
        PRINT #BMFile, "            IF vwatch64_PREVVARIABLES(i) <> vwatch64_VARIABLES(i).VALUE THEN"
        PRINT #BMFile, "                vwatch64_PREVVARIABLES(i) = vwatch64_VARIABLES(i).VALUE"
        PRINT #BMFile, "                PRINT #vwatch64_LOGFILE, "
        PRINT #BMFile, "                PRINT #vwatch64_LOGFILE, " + Q$ + "Value changed:" + Q$
        PRINT #BMFile, "                PRINT #vwatch64_LOGFILE, SPACE$(4) + RTRIM$(vwatch64_VARIABLES(i).NAME) + " + Q$ + "(" + Q$ + " + RTRIM$(vwatch64_VARIABLES(i).SCOPE) + " + Q$ + " " + Q$ + " + RTRIM$(vwatch64_VARIABLES(i).DATATYPE) + " + Q$ + ")" + Q$
        PRINT #BMFile, "                PRINT #vwatch64_LOGFILE, " + Q$ + "    = " + Q$ + " + RTRIM$(vwatch64_VARIABLES(i).VALUE)"
        PRINT #BMFile, "            END IF"
        PRINT #BMFile, "        NEXT i"
        PRINT #BMFile, "    END IF"
        PRINT #BMFile, ""
        PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_DATABLOCK, vwatch64_VARIABLES()"
        PRINT #BMFile, "END SUB"
        PRINT #BMFile, ""
    END IF
    PRINT #BMFile, "SUB vwatch64_CHECKBREAKPOINT (LineNumber AS LONG)"
    PRINT #BMFile, "    STATIC FirstRunDone AS _BIT"
    PRINT #BMFile, "    STATIC StepMode AS _BIT"
    PRINT #BMFile, "    STATIC RunCount AS INTEGER"
    PRINT #BMFile, ""
    PRINT #BMFile, "    IF vwatch64_HEADER.HISTORY_LOG = -1 THEN"
    PRINT #BMFile, "        RunCount = RunCount + 1"
    PRINT #BMFile, "        PRINT #vwatch64_LOGFILE, STR$(LineNumber); "
    PRINT #BMFile, "        IF RunCount = 30 THEN RunCount = 0: PRINT #vwatch64_LOGFILE,"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, ""
    PRINT #BMFile, "    IF FirstRunDone = 0 THEN"
    PRINT #BMFile, "        IF vwatch64_HEADER.CONNECTED = 0 THEN"
    PRINT #BMFile, "            _DELAY .5"
    PRINT #BMFile, "            _TITLE " + Q$ + "Untitled" + Q$
    PRINT #BMFile, "            FirstRunDone = -1"
    PRINT #BMFile, "            EXIT SUB"
    PRINT #BMFile, "        END IF"
    PRINT #BMFile, "    ELSE"
    PRINT #BMFile, "        IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT SUB"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, ""
    PRINT #BMFile, "    vwatch64_CLIENT.LINENUMBER = LineNumber"
    PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT"
    PRINT #BMFile, ""
    PRINT #BMFile, "    'Check if step mode was initiated by the host:"
    PRINT #BMFile, "    GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #BMFile, "    IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1"
    PRINT #BMFile, ""
    PRINT #BMFile, "    'Indicate to the host we're ready to go and get the breakpoint list:"
    PRINT #BMFile, "    vwatch64_HEADER.CLIENT_PING = -1"
    PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "    vwatch64_BREAKPOINT.ACTION = vwatch64_READY"
    PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #BMFile, "    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTLISTBLOCK, vwatch64_BREAKPOINTLIST"
    PRINT #BMFile, ""
    IF TotalSelected > 0 THEN
        PRINT #BMFile, "    vwatch64_VARIABLEWATCH"
    END IF
    PRINT #BMFile, ""
    PRINT #BMFile, "    'On the first time this procedure is called, execution is halted,"
    PRINT #BMFile, "    'until the user presses F5 or F8 in vWATCH64"
    PRINT #BMFile, "    IF FirstRunDone = 0 THEN"
    PRINT #BMFile, "        'It is safe to change the client's title at this point because"
    PRINT #BMFile, "        'it's the first line to be run so no _TITLE has yet been set."
    PRINT #BMFile, "        _TITLE " + Q$ + "Switch to vWATCH64 and hit F5 to run; F8 to step through;" + Q$
    PRINT #BMFile, "        FirstRunDone = -1"
    PRINT #BMFile, "        VWATCH64_STOPTIMERS"
    PRINT #BMFile, "        DO: _LIMIT 500"
    PRINT #BMFile, "            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #BMFile, "            GOSUB vwatch64_PING"
    PRINT #BMFile, "        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP"
    PRINT #BMFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1"
    PRINT #BMFile, "        VWATCH64_STARTTIMERS"
    PRINT #BMFile, "        _TITLE " + Q$ + "Untitled" + Q$
    PRINT #BMFile, "        EXIT SUB"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, ""
    PRINT #BMFile, "    IF (ASC(vwatch64_BREAKPOINTLIST, LineNumber) = 1) OR (StepMode = -1) THEN"
    PRINT #BMFile, "        VWATCH64_STOPTIMERS"
    PRINT #BMFile, "        StepMode = -1"
    PRINT #BMFile, "        DO: _LIMIT 500"
    PRINT #BMFile, "            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT"
    PRINT #BMFile, "            GOSUB vwatch64_PING"
    PRINT #BMFile, "        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP"
    PRINT #BMFile, "        IF vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE THEN StepMode = 0"
    PRINT #BMFile, "        VWATCH64_STARTTIMERS"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, ""
    PRINT #BMFile, "    EXIT SUB"
    PRINT #BMFile, "    vwatch64_PING:"
    PRINT #BMFile, "    'Check if connection is still alive on host's end"
    PRINT #BMFile, "    GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "    IF vwatch64_HEADER.CONNECTED = 0 THEN CLOSE vwatch64_CLIENTFILE: EXIT SUB"
    PRINT #BMFile, "    IF vwatch64_HEADER.HOST_PING = 0 THEN"
    PRINT #BMFile, "        IF TIMER - vwatch64_LAST_PING# > vwatch64_TIMEOUTLIMIT THEN"
    PRINT #BMFile, "            vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "            CLOSE vwatch64_CLIENTFILE"
    PRINT #BMFile, "            VWATCH64_STARTTIMERS"
    PRINT #BMFile, "            EXIT SUB"
    PRINT #BMFile, "        END IF"
    PRINT #BMFile, "    ELSE"
    PRINT #BMFile, "        vwatch64_LAST_PING# = TIMER"
    PRINT #BMFile, "    END IF"
    PRINT #BMFile, "    vwatch64_HEADER.HOST_PING = 0"
    PRINT #BMFile, "    vwatch64_HEADER.CLIENT_PING = -1"
    PRINT #BMFile, "    PUT #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER"
    PRINT #BMFile, "    RETURN"
    PRINT #BMFile, "END SUB"
    CLOSE BMFile
    PRINT "Done."
    IF VERBOSE THEN SLEEP 1

    $IF WIN THEN
        ThisPath$ = ""
        ExecutableExtension$ = ".exe"
    $ELSE
        ThisPath$ = "./"
        ExecutableExtension$ = ""
    $END IF
    Compiler$ = "qb64" + ExecutableExtension$

    IF NOT DONTCOMPILE AND _FILEEXISTS(Compiler$) THEN
        PRINT "Attempting to compile...";
        AttemptCompile% = SHELL(ThisPath$ + Compiler$ + " -c " + Q$ + NEWFILENAME$ + Q$)
        IF AttemptCompile% <> 0 THEN
            PRINT "failed (error code: "; TRIM$(STR$(AttemptCompile%)); ")"
            PRINT "Files have been generated, you will have to compile them yourself."
            PRINT "Press any key to go back..."
            SLEEP
            EXIT SUB
        ELSE
            PRINT "done."
            IF _FILEEXISTS(LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$) THEN
                SHELL _DONTWAIT ThisPath$ + LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$
            ELSE
                PRINT "Could not run "; LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$ + "."
                PRINT "You will have to compile/run it yourself."
                PRINT "Press any key to go back..."
                SLEEP
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
        FOR cb = 1 TO TotalButtons
            IF (mx >= Buttons(cb).X) AND (mx <= Buttons(cb).X + Buttons(cb).W) THEN
                IF (my >= Buttons(cb).Y) AND (my < Buttons(cb).Y + _FONTHEIGHT) THEN
                    WHILE _MOUSEBUTTON(1): _LIMIT 500: mb = _MOUSEINPUT: WEND
                    mb = 0: mx = _MOUSEX: my = _MOUSEY
                    IF Dialog = 1 THEN
                        SELECT CASE cb
                            CASE 1: SKIPARRAYS = NOT SKIPARRAYS: RETURN
                            CASE 2: INTERACTIVE = NOT INTERACTIVE: RETURN
                            CASE 3: DONTCOMPILE = NOT DONTCOMPILE: RETURN
                            CASE 4: VERBOSE = NOT VERBOSE: RETURN
                            CASE 5: DIALOGRESULT = 1: RETURN
                            CASE 6: DIALOGRESULT = 2: RETURN
                            CASE ELSE
                                BEEP 'in case a button was added but not yet assigned
                        END SELECT
                    ELSE
                        OVERWRITE = cb: RETURN
                    END IF
                    RETURN
                END IF
            END IF
        NEXT cb
    END IF
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
FUNCTION FINDVARIABLES (Text$, AddedList$)
    FOR i = 1 TO TOTALVARIABLES
        IF TRIM$(VARIABLES(i).NAME) = TRIM$(Text$) AND ASC(AddedList$, i) = 1 THEN
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
    IF INSTR(_OS$, "WIN") THEN div$ = "\" ELSE div$ = "/"
    IF INSTR(FILENAME$, div$) = 0 THEN NOPATH$ = FILENAME$: EXIT FUNCTION
    FOR i = LEN(FILENAME$) TO 1 STEP -1
        c$ = MID$(FILENAME$, i, 1)
        IF c$ = div$ THEN
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
    TRIM$ = RTRIM$(LTRIM$(TRUNCATE$(Text$, CHR$(0))))
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION TRUNCATE$ (Text$, Char$)
    FOR i = 1 TO LEN(Text$)
        IF MID$(Text$, i, 1) = Char$ THEN EXIT FOR
        NewText$ = NewText$ + MID$(Text$, i, 1)
    NEXT i
    TRUNCATE$ = NewText$
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
            SUFFIXLOOKUP$ = "FLOAT"
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
FUNCTION GETNEXTVARIABLE$ (Text$)
    'Parses a line of code in which more than one variable
    'may have been defined using commas. Returns an empty
    'string if there are no more variables in the line.

    DIM InBrackets AS INTEGER
    STATIC LastInput$
    STATIC Position%
    STATIC EndOfStatement AS _BIT

    Result$ = ""
    IF EndOfStatement THEN
        EndOfStatement = 0
        GETNEXTVARIABLE$ = ""
        EXIT FUNCTION
    END IF

    IF LastInput$ <> Text$ THEN
        'First time this line is passed
        Position% = 1
        LastInput$ = Text$

        IF UCASE$(LEFT$(LastInput$, 4)) = "DIM " THEN
            Position% = 4
            IF MID$(LastInput$, 5, 7) = "SHARED " THEN Position% = 11
        END IF
    END IF

    DO
        Position% = Position% + 1
        IF Position% > LEN(LastInput$) THEN EXIT DO
        Char$ = MID$(LastInput$, Position%, 1)
        SELECT CASE Char$
            CASE "(": InBrackets = InBrackets + 1
            CASE ")": InBrackets = InBrackets - 1
            CASE ",": IF InBrackets = 0 THEN EXIT DO
            CASE ":": EndOfStatement = -1: EXIT DO
            CASE "_": IF Position% = LEN(LastInput$) THEN EXIT DO
        END SELECT
        Result$ = Result$ + Char$
    LOOP

    GETNEXTVARIABLE$ = TRIM$(Result$)
END FUNCTION


'------------------------------------------------------------------------------
SUB SETUP_CONNECTION
    _KEYCLEAR 'Clears the keyboard buffer

    TotalButtons = 2
    DIM Buttons(1 TO TotalButtons) AS BUTTONSTYPE
    b = 1
    Buttons(b).CAPTION = "<Open and Process .BAS>": b = b + 1
    Buttons(b).CAPTION = "<ESC = Exit>": b = b + 1

    StartSetup:
    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)

    CLOSE #FILE
    FILE = FREEFILE
    ON ERROR GOTO FileError
    'Try killing vwatch64.dat. Won't work if open, so we'll try to reconnect to client.
    KILL PATHONLY$(EXENAME) + "vwatch64.dat"
    ON ERROR GOTO 0

    'Opens "vwatch64.dat" to wait for a connection:
    OPEN PATHONLY$(EXENAME) + "vwatch64.dat" FOR BINARY AS #FILE

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
        KILL PATHONLY$(EXENAME) + "vwatch64.dat"
        ON ERROR GOTO 0
        SYSTEM
    END IF

    _AUTODISPLAY
    CLS , _RGB32(255, 255, 255)
    'Connected! Check if client is compatible:
    IF HEADER.CLIENT_ID <> ID OR HEADER.VERSION <> VERSION THEN
        BEEP
        PRINT "Client not compatible."
        PRINT "Attempted connection by client with ID "; CHR$(34); HEADER.CLIENT_ID + CHR$(34)
        PRINT "Reported version: "; HEADER.VERSION
        PRINT "Press any key to go back..."
        SLEEP
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
        IF TIMER - Start# > 3 THEN EXIT DO
    LOOP UNTIL LEN(TRIM$(CLIENT.CHECKSUM)) > 0

    'No CHECKSUM received = connection closed.
    IF LEN(TRIM$(CLIENT.CHECKSUM)) = 0 THEN BEEP: GOTO StartSetup

    REDIM VARIABLES(1 TO CLIENT.TOTALVARIABLES) AS VARIABLESTYPE
    BREAKPOINTLIST = STRING$(CLIENT.TOTALSOURCELINES, 0)
    DATABLOCK = BREAKPOINTLISTBLOCK + LEN(BREAKPOINTLIST) + 1

    'Load the source file, if it still exists.
    IF _FILEEXISTS(TRIM$(CLIENT.NAME)) THEN
        SOURCEFILENUM% = FREEFILE
        OPEN TRIM$(CLIENT.NAME) FOR BINARY AS SOURCEFILENUM%
        SOURCEFILE = SPACE$(LOF(SOURCEFILENUM%))
        GET #SOURCEFILENUM%, 1, SOURCEFILE

        SEEK #SOURCEFILENUM%, 1

        IF CLIENT.CHECKSUM <> ADLER32(SOURCEFILE) THEN
            SOURCEFILE = ""
        ELSE
            IF INSTR(SOURCEFILE, CHR$(13)) THEN LF = 13 ELSE LF = 10
            REDIM LINE_STARTS(1 TO CLIENT.TOTALSOURCELINES) AS LONG

            'Scan the file for line starts:
            CurrentLineNo = 1
            LONGESTLINE = 1
            DO
                NextLineStart = SEEK(SOURCEFILENUM%)
                LINE_STARTS(CurrentLineNo) = NextLineStart
                LINE INPUT #SOURCEFILENUM%, bkpSourceLine$
                IF LEN(bkpSourceLine$) > LONGESTLINE THEN LONGESTLINE = LEN(bkpSourceLine$)
                CurrentLineNo = CurrentLineNo + 1
            LOOP UNTIL EOF(SOURCEFILENUM%)
        END IF
        CLOSE SOURCEFILENUM%
    ELSE
        SOURCEFILE = ""
    END IF

    TITLESTRING = TITLESTRING + " - " + NOPATH$(TRIM$(CLIENT.NAME)) + IIFSTR$(LEN(TRIM$(CLIENT.EXENAME)), " (" + TRIM$(CLIENT.EXENAME) + ")", "")
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
                IF INSTR(Buttons(cb).CAPTION, "ESC =") THEN MENU% = 102: RETURN
                BEEP 'in case a button was added but not yet assigned
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

    IF INSTR(_OS$, "WIN") > 0 THEN LF$ = CHR$(13) + CHR$(10) ELSE LF$ = CHR$(10)

    LibOutput = FREEFILE
    FILEERRORRAISED = 0
    ON ERROR GOTO FileError
    OPEN "timers.h" FOR OUTPUT AS #LibOutput
    IF FILEERRORRAISED THEN BEEP: PRINT "Cannot write timers.h to "; _CWD$: SLEEP: SYSTEM

    SourceLine$ = "extern int32 ontimerthread_lock;" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "void stop_timers() {" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "  ontimerthread_lock = 1;" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "  while (ontimerthread_lock != 2);" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "}" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "void start_timers() {" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "  ontimerthread_lock = 0;" + LF$: PRINT #LibOutput, SourceLine$;
    SourceLine$ = "}" + LF$: PRINT #LibOutput, SourceLine$;

    CLOSE #LibOutput
END SUB

'------------------------------------------------------------------------------
FUNCTION ADLER32$ (DataArray$)
    'This function comes from Videogamer555. Read the original topic below:
    'http://www.qb64.net/forum/index.php?topic=2804.msg24245#msg24245
    DIM A32$
    DIM Astr AS STRING * 4
    DIM Bstr AS STRING * 4
    A = 1
    B = 0

    FOR i = 1 TO LEN(DataArray$)
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
    DIM LineLength AS LONG

    IF TargetLine = 0 THEN EXIT FUNCTION
    IF TargetLine > CLIENT.TOTALSOURCELINES THEN EXIT FUNCTION

    IF TargetLine < CLIENT.TOTALSOURCELINES THEN
        LineLength = LINE_STARTS(TargetLine + 1) - LINE_STARTS(TargetLine) - 1
        IF LF = 13 THEN LineLength = LineLength - 1
    ELSE
        LineLength = LEN(SOURCEFILE) - LINE_STARTS(TargetLine) - 1
    END IF

    GETLINE$ = MID$(SOURCEFILE, LINE_STARTS(TargetLine), LineLength)
END FUNCTION

'------------------------------------------------------------------------------
SUB SEND_PING
    'Check if the connection is still alive on the client's end
    GET #FILE, HEADERBLOCK, HEADER
    IF HEADER.CLIENT_PING = 0 THEN
        IF FIND_KEYWORD(GETLINE$(CLIENT.LINENUMBER), "INPUT", FoundAt) THEN LAST_PING# = TIMER
        IF FIND_KEYWORD(GETLINE$(CLIENT.LINENUMBER), "SLEEP", FoundAt) THEN LAST_PING# = TIMER
        IF FIND_KEYWORD(GETLINE$(CLIENT.LINENUMBER), "SHELL", FoundAt) THEN LAST_PING# = TIMER
        IF FIND_KEYWORD(GETLINE$(CLIENT.LINENUMBER), "_DELAY", FoundAt) THEN LAST_PING# = TIMER
        IF TIMER - LAST_PING# > TIMEOUTLIMIT THEN
            TIMED_OUT = -1
        END IF
    ELSE
        LAST_PING# = TIMER
        HEADER.CLIENT_PING = 0
        PUT #FILE, HEADERBLOCK, HEADER
    END IF

    'Inform the client we're still alive and kicking.
    HEADER.HOST_PING = -1
    PUT #FILE, HEADERBLOCK, HEADER
END SUB

'------------------------------------------------------------------------------
FUNCTION FIND_KEYWORD (Text$, SearchTerm$, SearchTermFound)
    SEP$ = " =<>+-/\^:;,*()!#$%&`"
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
'$INCLUDE:'glinput.bi'

