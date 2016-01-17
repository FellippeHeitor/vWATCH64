'vWATCH64 - A variable watch system for QB64 programs
'Fellippe Heitor, 2015 - fellippeheitor@gmail.com - @fellippeheitor
'
'Use this program when you need runtime verification of variable values
'in a program. The output will show you in real time variable changes
'inside your RUNNING program.
'
'To achieve this result, vWATCH64 will parse your .BAS and extract
'variable names. With the data gathered, a .BI and a .BM will be
'generated. A new .BAS file will be output with the appropriate
'INCLUDEd lines. If QB64.EXE is found, the output file is then
'compiled. If compilation is sucessful, the variable monitor is
'started and connects to your program output, displaying variable
'values in real time.
'
' - Beta 1: Initial release (December 13th, 2015)
'
' - Beta 2: December 14th, 2015
'     - Added code to look for and parse user defined types. Variables
'       SHARED as a user defined type can be monitored, unless they
'       are arrays.
'     - Fixed FILE being set as FREEFILE, which could conflict
'       with programs using OPEN "file" AS #1, instead of a variable
'       and FREEFILE.
'     - Added a STRIPCOMMENTS function, that, you guessed it,
'       strips away comments from a source line. I added this after
'       trying some other people's codes (especially Terry Ritchie's
'       games) and finding many comments being misinterpreted as
'       data types.
'
' - Beta 3: December 18th, 2015
'     - Checks to see if client is still sending data, if not, disconnects
'       (triggers a timeout error).
'     - Changed variable naming scheme, which allows for vWATCH64 to watch
'       itself.
'     - Fixed scroll bar for when there are too many variables on screen.
'     - Uses a true type font under Windows (Courier New Monospaced).
'     - Considers that not everyone may have used the IDE and thus not
'       every keyword will be in CAPITALS.
'     - Parses COMMAND$ to show only the file name in _TITLE; also, to be able
'       to use the original path given to place .BI, .BM and the new .BAS.
'       Output is now quieter while processing, unless -v is used in command
'       line.
'     - Connects to an already running client, if compatible - useful for
'       when vWATCH64 is closed by accident before the client.
'     - Checks if destination file already exists before processing.
'     - Checks if source = destination, to avoid overwriting original
'       source file.
'     - Looks for type suffixes (sigils - $, %, &, !, etc) and parse them.
'       (`n and $n are supported, but validity of values won't be checked).
'     - Can now watch local variables from MODULE level, acessing them through
'       the SHARED keyword from inside sub vwatch64_VARIABLEWATCH in the .BM
'     - Looks for variables with no explicit data type (assumes SINGLE
'       or as otherwise set by DEF)
'     - Parses lines with more than one variable defined (using commas)
'
' - Beta 4: December 20th, 2015
'     - Deletes vwatch64.dat upon exit (before it was only deleted after client
'       was intentionally closed it or after the connection timed out.
'     - Allows user to filter the list of variables by name, type or value.
'       Just start typing to start filtering. TAB to change fields.
'     - Allows the user to drag the scrollbar (mousewheel is reportedly broken
'       under Linux)
'     - Adjusted scroll increment for long lists.
'     - Fixed: connects to an already running client, if compatible - useful for
'       when vWATCH64 is closed by accident before the client.
'     - Shows the .EXE file name being monitored on the title bar. (Windows only)
'     - Added automatic compilation under MAC OS X (tested) and under Linux
'       (expected to work with no changes).
'     - Fixed: module-level variables defined as a User Defined Type generated
'       an error. Now they can be watched as well.
'     - Added support for multiline statements (ending with an underscore).
'
' - Beta 5: December 23rd, 2015
'     - Code is now a bit easier to read. Sections have been moved to SUBs, making
'       it easier to follow.
'     - Scroll bar behavior has been completely recoded and now works as expected
'       (scrollbars only show when the list is too long to fit the program area)
'     - Static arrays can now be monitored. vWATCH64 will look for unidimensional
'       arrays and add them to the watch list.
'     - Lower boundary of arrays is determined based on OPTION BASE, if found in
'       the source file being processed.
'     - Command line switches now include -dontcompile and -noarrays
'
' - Beta 6: December 25th, 2015
'     - Now uses Steve's File Selection Utility v1.2 - grab it here:
'       http://www.qb64.net/forum/index.php?topic=11253.0
'     - Also uses Terry Ritchie's QB64 Menu and GLInput libraries - grab them here:
'       https://dl.dropbox.com/u/416997/AllRitchiesQB64Libraries.zip
'     - Doesn't require command line to set flags anymore. User can answer Y/N
'       questions before processing of files.
'
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

'$INCLUDE:'menutop.bi'
'$INCLUDE:'glinputtop.bi'

'Constants: -------------------------------------------------------------------
CONST ID = "vWATCH64"
CONST VERSION = "0.6b "

CONST FALSE = 0
CONST TRUE = NOT FALSE

CONST TIMEOUTLIMIT = 3

'Filters:
CONST VARIABLENAMES = 1
CONST VALUES = 2
CONST DATATYPES = 3

'Custom data types: -----------------------------------------------------------
TYPE HEADERTYPE
    HOST_ID AS STRING * 8
    VERSION AS STRING * 5
    CONNECTED AS _BYTE
END TYPE

TYPE CLIENTTYPE
    NAME AS STRING * 256
    EXENAME AS STRING * 256
    CURRENTMODULE AS STRING * 50
    LASTOUTPUT AS DOUBLE
    TOTALVARIABLES AS INTEGER
END TYPE

TYPE VARIABLESTYPE
    NAME AS STRING * 40
    SCOPE AS STRING * 7
    UDT AS STRING * 40
    DATATYPE AS STRING * 20
    VALUE AS STRING * 255
END TYPE

TYPE UDTTYPE
    UDT AS STRING * 40
    ELEMENT AS STRING * 40
    DATATYPE AS STRING * 20
END TYPE

'Shared variables: ------------------------------------------------------------
DIM SHARED MAINSCREEN AS LONG
DIM SHARED INFOSCREEN AS LONG
DIM SHARED INFOSCREENHEIGHT AS INTEGER
DIM SHARED FILE AS INTEGER
DIM SHARED USERQUIT AS _BIT
DIM SHARED RESPONSE AS STRING * 6
DIM SHARED PREVLOF AS LONG
DIM SHARED FILELENGTH AS LONG
DIM SHARED DATABLOCK AS LONG
DIM SHARED CLIENTDATA AS LONG
DIM SHARED HEADER AS HEADERTYPE
DIM SHARED CLIENT AS CLIENTTYPE
DIM SHARED TITLESTRING AS STRING
DIM SHARED INTERNALKEYWORDS AS INTEGER
DIM SHARED DEFAULTDATATYPE AS STRING * 20
DIM SHARED VERBOSE AS _BIT
DIM SHARED DONTCOMPILE AS _BIT
DIM SHARED EXENAME AS STRING
DIM SHARED TOTALVARIABLES AS INTEGER
DIM SHARED MULTILINE AS _BIT
DIM SHARED TTFONT AS LONG
DIM SHARED OPTIONBASE AS INTEGER
DIM SHARED SKIPARRAYS AS _BIT
DIM SHARED MENU%
DIM SHARED hWnd&

DIM OVERLAYSCREEN AS LONG
DIM TIMEOUT AS _BYTE
DIM i AS INTEGER

'Variables initialization: ----------------------------------------------------
DEFAULTDATATYPE = "SINGLE"
OPTIONBASE = 0
VERBOSE = 0
DONTCOMPILE = 0
SKIPARRAYS = 0

'Screen setup: ----------------------------------------------------------------
MAINSCREEN = _NEWIMAGE(1000, 600, 32)
SCREEN MAINSCREEN

$IF WIN THEN
    'Under Windows, if Courier font is found, it is used;
    'Otherwise we stick to _FONT 16 (default):
    TTFONT = _LOADFONT("C:\windows\fonts\cour.ttf", 14, "MONOSPACE, BOLD")
    IF TTFONT THEN _FONT TTFONT
    Ret = GetModuleFileNameA(0, EXENAME_HOLDER$256, LEN(EXENAME_HOLDER$256))
    IF Ret > 0 THEN
        EXENAME = LEFT$(EXENAME_HOLDER$256, Ret)
    END IF
$ELSE
    EXENAME = ""
$END IF

RESTORE MenuDATA
MAKEMENU

'Parse the command line: ------------------------------------------------------
'Did the user drag a .BAS file onto this program or enter parameters?
'Syntax: VWATCH64 [source filename.bas] [destination filename] [-v] [-dontcompile]
'(-v is for Verbose mode while processing - only works if file names are provided)
'If no parameters are passed, vWATCH64 starts in MONITOR MODE
IF LEN(COMMAND$) THEN
    IF _COMMANDCOUNT > 1 THEN
        'Set flags based on command line arguments
        FOR i = 2 TO _COMMANDCOUNT
            SELECT CASE LCASE$(COMMAND$(i))
                CASE "-v": VERBOSE = -1 'Verbose switch
                CASE "-dontcompile": DONTCOMPILE = -1
                CASE "-noarrays": SKIPARRAYS = -1
                CASE ELSE
                    'Any other arguments are ignored.
            END SELECT
        NEXT i
    END IF

    IF _FILEEXISTS(COMMAND$(1)) THEN PROCESSFILE COMMAND$(1) ELSE BEEP
END IF

GOTO MonitorMode
OpenFileMenu:
FILENAME$ = SelectFile$("*.BAS;*.*", _WIDTH(MAINSCREEN) / 2 - 320, _HEIGHT(MAINSCREEN) / 2 - 240)
_AUTODISPLAY
IF _FILEEXISTS(FILENAME$) THEN PROCESSFILE (FILENAME$) ELSE BEEP

'------------------------------------------------------------------------------
MonitorMode:
'------------------------------------------------------------------------------
TITLESTRING = "vWATCH64 - v" + VERSION
_TITLE TITLESTRING
COLOR _RGB32(0, 0, 0), _RGB32(255, 255, 255)
CLS

SETUP_CONNECTION
IF MENU% = 101 THEN GOTO OpenFileMenu
REDIM SHARED VARIABLES(1 TO CLIENT.TOTALVARIABLES) AS VARIABLESTYPE
MONITOR_MODE
'------------------------------------------------------------------------------

IF NOT USERQUIT THEN GOTO MonitorMode
SYSTEM

FileError:
RESUME NEXT

MenuDATA:
DATA "&File","&Open and process .BAS...","-E&xit","*"
DATA "!"

KeyWordsDATA:
DATA _BIT,_UNSIGNED _BIT,_BYTE,_UNSIGNED _BYTE,INTEGER
DATA _UNSIGNED INTEGER,LONG,_UNSIGNED LONG,_INTEGER64
DATA _UNSIGNED _INTEGER64,SINGLE,DOUBLE,_FLOAT,STRING
DATA END

'$INCLUDE:'menu.bi'
'$INCLUDE:'glinput.bi'

'------------------------------------------------------------------------------
'SUBs and FUNCTIONs:                                                          -
'------------------------------------------------------------------------------
SUB WAITFORDATA
    'Waits until data is put in a binary file. We monitor the length of
    'the file with LOF(FILE) until it is larger than the
    'previously reported length (PREVLOF), which indicates
    'new data was PUT/PRINTed
    Start# = TIMER
    DO: _LIMIT 30
        FILELENGTH = LOF(FILE)
        IF FILELENGTH > PREVLOF THEN
            'Data received!
            PREVLOF = FILELENGTH
            EXIT DO
        END IF
        IF _KEYHIT = 27 THEN EXIT DO
        IF TIMER - Start# > TIMEOUTLIMIT THEN EXIT DO
    LOOP
END SUB

'------------------------------------------------------------------------------
SUB PROCESSFILE (FILENAME$)
    'Parses a .BAS file and reads all compatible variables
    'in order to generate a compatible vWATCH64 client.

    DIM InputFile AS INTEGER
    DIM OutputFile AS INTEGER
    DIM BIFile AS INTEGER
    DIM BIFileName AS STRING
    DIM BMFile AS INTEGER
    DIM BMFileName AS STRING
    DIM SourceLine AS STRING
    DIM caseBkpSourceLine AS STRING
    DIM TotalLocalVariables AS INTEGER
    DIM TotalKeywords AS INTEGER
    DIM TotalUDTs AS INTEGER
    DIM TotalUDTsAdded AS INTEGER
    DIM TotalLines AS LONG
    DIM ThisKeyword AS STRING
    DIM DefiningType AS _BIT
    DIM DeclaringLibrary AS _BIT
    DIM FoundType AS STRING
    DIM MainModule AS _BYTE
    DIM LocalVariable AS _BIT
    DIM IsArray AS _BIT
    DIM bkpSourceLine$
    DIM NextVar$
    DIM caseBkpNextVar$
    DIM DefaultTypeUsed AS _BIT
    REDIM UDT(1) AS UDTTYPE, UDT_ADDED(1) AS VARIABLESTYPE
    REDIM VARIABLES(1) AS VARIABLESTYPE
    REDIM LOCALVARIABLES(1) AS VARIABLESTYPE
    REDIM LOCALSHAREDADDED(1) AS STRING
    REDIM KeywordList(1) AS STRING

    RESTORE KeyWordsDATA
    'Populate KeywordList() with DATA TYPES
    DO
        READ ThisKeyword
        IF UCASE$(ThisKeyword) = "END" THEN
            INTERNALKEYWORDS = TotalKeywords
            EXIT DO
        END IF
        GOSUB AddThisKeyword
    LOOP

    Q$ = CHR$(34)

    'Process dialog
    DialogX = _WIDTH(MAINSCREEN) / 2 - 200
    DialogY = _HEIGHT(MAINSCREEN) / 2 - 100
    CLS , _RGB32(255, 255, 255)
    LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF

    COLOR _RGB32(0, 0, 0), _RGB32(200, 200, 200)
    _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
    _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
    getfilename% = GLIINPUT(DialogX + 5, DialogY + 5 + _FONTHEIGHT * 2, GLIALPHA + GLINUMERIC + GLIDASH, "New file name: ", TRUE)
    DO
        GLICLEAR
        GLIUPDATE
        _DISPLAY
    LOOP UNTIL GLIENTERED(getfilename%)
    _AUTODISPLAY
    NEWFILENAME$ = GLIOUTPUT$(getfilename%)
    GLICLOSE getfilename%, FALSE

    'Check if processing can proceed:
    IF LEN(TRIM$(NEWFILENAME$)) = 0 THEN
        EXIT SUB
    END IF

    BIFileName = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4), NEWFILENAME$) + ".BI"
    BMFileName = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4), NEWFILENAME$) + ".BM"
    NEWFILENAME$ = PATHONLY$(FILENAME$) + IIFSTR$(UCASE$(RIGHT$(NEWFILENAME$, 4)) = ".BAS", NEWFILENAME$, NEWFILENAME$ + ".BAS")

    IF UCASE$(FILENAME$) = UCASE$(NEWFILENAME$) THEN
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 3), "Source file = destination file. Can't proceed."
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 4), "Press any key to continue..."
        BEEP
        SLEEP
        EXIT SUB
    END IF

    IF _FILEEXISTS(NEWFILENAME$) THEN
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 3), NOPATH$(NEWFILENAME$) + " already exists."
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 4), "Overwrite (Y/N)?"
        BEEP
        DO
            k = _KEYHIT
        LOOP UNTIL k = 89 OR k = 121 OR k = 78 OR k = 110
        IF k = 78 OR k = 110 THEN EXIT SUB
    END IF

    'Options dialogs, unless already set using command line
    IF _COMMANDCOUNT <= 1 THEN
        CLS , _RGB32(255, 255, 255)
        LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF
        _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 3), "Include arrays (Y/N)?"
        _KEYCLEAR
        DO
            k = _KEYHIT
        LOOP UNTIL k = 89 OR k = 121 OR k = 78 OR k = 110
        IF k = 78 OR k = 110 THEN SKIPARRAYS = -1 ELSE SKIPARRAYS = 0
    END IF

    IF _COMMANDCOUNT <= 1 THEN
        CLS , _RGB32(255, 255, 255)
        LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF
        _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 3), "Attempt to compile in the end (Y/N)?"
        _KEYCLEAR
        DO
            k = _KEYHIT
        LOOP UNTIL k = 89 OR k = 121 OR k = 78 OR k = 110
        IF k = 78 OR k = 110 THEN DONTCOMPILE = -1 ELSE DONTCOMPILE = 0
    END IF

    IF _COMMANDCOUNT <= 1 THEN
        CLS , _RGB32(255, 255, 255)
        LINE (DialogX, DialogY)-STEP(400, 200), _RGB32(200, 200, 200), BF
        _PRINTSTRING (DialogX + 5, DialogY + 5), "vWATCH64 - v" + VERSION
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT), "Processing file: " + NOPATH$(FILENAME$)
        _PRINTSTRING (DialogX + 5, DialogY + 5 + _FONTHEIGHT * 3), "Show processing details (Y/N)?"
        _KEYCLEAR
        DO
            k = _KEYHIT
        LOOP UNTIL k = 89 OR k = 121 OR k = 78 OR k = 110
        IF k = 78 OR k = 110 THEN VERBOSE = 0 ELSE VERBOSE = -1
    END IF


    'Processing can proceed.
    COLOR _RGB32(0, 0, 0), _RGB32(230, 230, 230)
    CLS
    InputFile = FREEFILE
    OPEN FILENAME$ FOR BINARY AS #InputFile

    OutputFile = FREEFILE
    OPEN NEWFILENAME$ FOR OUTPUT AS #OutputFile

    MainModule = -1
    'Injects the required code into processed file:
    PRINT #OutputFile, "'$INCLUDE:'" + BIFileName + "'"

    'Looks for variables inside the main module and stores information in VARIABLES()
    'and LOCALVARIABLES. If SUB or FUNCTION is found, injects CURRENTMODULE verification
    'code. If SYSTEM is found, injects cleanup procedures (also when main module ends).
    TOTALVARIABLES = 0
    PRINT "Parsing .BAS...";
    IF VERBOSE THEN PRINT
    row = CSRLIN: col = POS(1)
    MULTILINE = 0
    DO
        IF LEN(caseBkpNextVar$) = 0 THEN 'Reads next line from file unless we're in the middle of processing a line
            LINE INPUT #InputFile, bkpSourceLine$ 'Reads the next source line
            caseBkpSourceLine = TRIM$(STRIPCOMMENTS(bkpSourceLine$)) 'Generates a version without comments or extra spaces
            SourceLine = UCASE$(caseBkpSourceLine) 'Generates an all upper case version
            IF NOT VERBOSE THEN LOCATE row, col: PRINT USING "###"; (SEEK(InputFile) / LOF(InputFile)) * 100;: PRINT "% (Watchable variables found: "; TRIM$(STR$(TOTALVARIABLES)); ")"
        ELSE
            NextVar$ = UCASE$(caseBkpNextVar$)
        END IF

        IF DefiningType THEN
            'A TYPE ... was found earlier, so until we find an END TYPE, we'll populate UDT()
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
                        PRINT "Found UDT: "; RTRIM$(UDT(TotalUDTs).UDT); "."; RTRIM$(UDT(TotalUDTs).ELEMENT); " AS "; RTRIM$(UDT(TotalUDTs).DATATYPE)
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
                    FoundType = DEFAULTDATATYPE 'Assumes default data type
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
                    PRINT RTRIM$(VARIABLES(TOTALVARIABLES).NAME)
                    _DELAY .05
                END IF
            ELSE
                FoundType = RIGHT$(NextVar$, LEN(NextVar$) - INSTR(NextVar$, " AS ") - 3)

                IF CHECKLIST(FoundType, KeywordList(), INTERNALKEYWORDS) THEN
                    'Variable is defined as an internal DATA TYPE

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
                        PRINT RTRIM$(VARIABLES(TOTALVARIABLES).NAME)
                        _DELAY .05
                    END IF
                ELSE
                    'Variable is defined as a user defined type
                    IsArray = 0
                    IF INSTR(NextVar$, "(") THEN IsArray = -1: PARSEARRAY NextVar$, ValidArray%, LowerBoundary%, UpperBoundary%
                    IF IsArray THEN
                        IF ValidArray% AND NOT SKIPARRAYS THEN
                            ValidArray% = 0
                            FOR ItemsinArray = LowerBoundary% TO UpperBoundary%
                                FOR i = 1 TO TotalUDTs
                                    'Expand variables defined as UDTs to Variable.Element format:
                                    IF UCASE$(RTRIM$(UDT(i).UDT)) = FoundType THEN
                                        TOTALVARIABLES = TOTALVARIABLES + 1
                                        REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                                        VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, "(")) + TRIM$(STR$(ItemsinArray)) + ")." + RTRIM$(UDT(i).ELEMENT)
                                        VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                                        VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                        VARIABLES(TOTALVARIABLES).DATATYPE = RTRIM$(UDT(i).DATATYPE)

                                        IF LocalVariable THEN
                                            TotalLocalVariables = TotalLocalVariables + 1
                                            REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                            LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                            LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                        END IF

                                        IF VERBOSE THEN
                                            PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                                            PRINT UDT(i).DATATYPE;
                                            PRINT RTRIM$(VARIABLES(TOTALVARIABLES).NAME)
                                            _DELAY .05
                                        END IF
                                    END IF
                                NEXT i
                            NEXT ItemsinArray
                        END IF
                    ELSE
                        FOR i = 1 TO TotalUDTs
                            'Expand variables defined as UDTs to Variable.Element format:
                            IF UCASE$(RTRIM$(UDT(i).UDT)) = FoundType THEN
                                TOTALVARIABLES = TOTALVARIABLES + 1
                                REDIM _PRESERVE VARIABLES(1 TO TOTALVARIABLES) AS VARIABLESTYPE
                                VARIABLES(TOTALVARIABLES).NAME = LEFT$(caseBkpNextVar$, INSTR(NextVar$, " AS ") - 1) + "." + RTRIM$(UDT(i).ELEMENT)
                                VARIABLES(TOTALVARIABLES).SCOPE = IIFSTR$(LocalVariable, "LOCAL", "SHARED")
                                VARIABLES(TOTALVARIABLES).UDT = UDT(i).UDT
                                VARIABLES(TOTALVARIABLES).DATATYPE = RTRIM$(UDT(i).DATATYPE)

                                IF LocalVariable THEN
                                    TotalLocalVariables = TotalLocalVariables + 1
                                    REDIM _PRESERVE LOCALVARIABLES(1 TO TotalLocalVariables) AS VARIABLESTYPE
                                    LOCALVARIABLES(TotalLocalVariables).NAME = VARIABLES(TOTALVARIABLES).NAME
                                    LOCALVARIABLES(TotalLocalVariables).DATATYPE = VARIABLES(TOTALVARIABLES).DATATYPE
                                END IF

                                IF VERBOSE THEN
                                    PRINT TOTALVARIABLES; IIFSTR$(LocalVariable, "LOCAL  ", "SHARED ");
                                    PRINT UDT(i).DATATYPE;
                                    PRINT RTRIM$(VARIABLES(TOTALVARIABLES).NAME)
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
            OPTIONBASE = 1
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFINT " THEN
            DEFAULTDATATYPE = "INTEGER"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFLNG " THEN
            DEFAULTDATATYPE = "LONG"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSTR " THEN
            DEFAULTDATATYPE = "STRING"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFSNG " THEN
            DEFAULTDATATYPE = "SINGLE"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 7) = "DEFDBL " THEN
            DEFAULTDATATYPE = "DOUBLE"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 8) = "_DEFINE " THEN
            IF INSTR(SourceLine, " AS ") > 0 THEN
                DEFAULTDATATYPE = RIGHT$(SourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3)
            END IF
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 5) = "TYPE " THEN
            'User defined types will be added to the DATA TYPE keyword list
            ThisKeyword = RIGHT$(caseBkpSourceLine, LEN(SourceLine) - 5)
            GOSUB AddThisKeyword
            DefiningType = -1
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF LEFT$(SourceLine, 4) = "SUB " THEN
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
                    PRINT #OutputFile, "    IF vwatch64_AUTHORIZED THEN"
                    PRINT #OutputFile, "        vwatch64_HEADER.CONNECTED = 0"
                    PRINT #OutputFile, "        PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
                    PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
                    PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
                    PRINT #OutputFile, "        KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
                    PRINT #OutputFile, "    END IF"
                    PRINT #OutputFile, ""
                    PRINT #OutputFile, "    vwatch64_FILEERROR:"
                    PRINT #OutputFile, "    RESUME NEXT"
                    PRINT #OutputFile, "$END IF"
                    PRINT #OutputFile,
                END IF
                PRINT #OutputFile, bkpSourceLine$
                PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
                IF INSTR(SourceLine, "(") THEN
                    IF VERBOSE THEN PRINT "Found: SUB "; MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5)
                    SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "SUB " + MID$(caseBkpSourceLine, 5, INSTR(SourceLine, "(") - 5) + Q$
                ELSE
                    IF VERBOSE THEN PRINT "Found: "; caseBkpSourceLine
                    SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + caseBkpSourceLine + Q$
                END IF
                PRINT #OutputFile, SourceLine
                PRINT #OutputFile, "$END IF"
                IF VERBOSE THEN _DELAY .05
            ELSE
                PRINT #OutputFile, bkpSourceLine$
            END IF
        ELSEIF LEFT$(SourceLine, 9) = "FUNCTION " THEN
            IF NOT DeclaringLibrary THEN
                IF MainModule THEN
                    MainModule = 0
                    PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
                    PRINT #OutputFile, "    IF vwatch64_AUTHORIZED THEN"
                    PRINT #OutputFile, "        vwatch64_HEADER.CONNECTED = 0"
                    PRINT #OutputFile, "        PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
                    PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
                    PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
                    PRINT #OutputFile, "        KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
                    PRINT #OutputFile, "    END IF"
                    PRINT #OutputFile, ""
                    PRINT #OutputFile, "    vwatch64_FILEERROR:"
                    PRINT #OutputFile, "    RESUME NEXT"
                    PRINT #OutputFile, "$END IF"
                    PRINT #OutputFile,
                END IF
                PRINT #OutputFile, bkpSourceLine$
                PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
                IF INSTR(SourceLine, "(") THEN
                    IF VERBOSE THEN PRINT "Found: FUNCTION "; MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10)
                    SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "FUNCTION " + MID$(caseBkpSourceLine, 10, INSTR(SourceLine, "(") - 10) + Q$
                ELSE
                    IF VERBOSE THEN PRINT caseBkpSourceLine
                    SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + caseBkpSourceLine + Q$
                END IF
                PRINT #OutputFile, SourceLine
                PRINT #OutputFile, "$END IF"
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
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF INSTR(SourceLine, "EXIT SUB") OR INSTR(SourceLine, "EXIT FUNCTION") THEN
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile, bkpSourceLine$
        ELSEIF SourceLine = "SYSTEM" OR SourceLine = "END" THEN
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    IF vwatch64_AUTHORIZED THEN"
            PRINT #OutputFile, "        vwatch64_HEADER.CONNECTED = 0"
            PRINT #OutputFile, "        PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
            PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
            PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
            PRINT #OutputFile, "        KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
            PRINT #OutputFile, "    END IF"
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile,
            PRINT #OutputFile, bkpSourceLine$
        ELSE
            PRINT #OutputFile, bkpSourceLine$
        END IF
    LOOP UNTIL EOF(InputFile)

    IF TOTALVARIABLES = 0 THEN
        BEEP
        PRINT "There are no watchable variables in the .BAS source."
        PRINT "Press any key to abort processing..."
        CLOSE
        KILL NEWFILENAME$
        SLEEP
        EXIT SUB
    ELSE
        PRINT "Total watchable variables found: "; TOTALVARIABLES
        IF VERBOSE THEN _DELAY .05
    END IF

    IF MainModule THEN 'All lines have been parsed. This .BAS contains no SUBs/FUNCTIONs.
        MainModule = 0
        PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
        PRINT #OutputFile, "    IF vwatch64_AUTHORIZED THEN"
        PRINT #OutputFile, "        vwatch64_HEADER.CONNECTED = 0"
        PRINT #OutputFile, "        PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
        PRINT #OutputFile, "        CLOSE #vwatch64_CLIENTFILE"
        PRINT #OutputFile, "        ON ERROR GOTO vwatch64_FILEERROR"
        PRINT #OutputFile, "        KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
        PRINT #OutputFile, "    END IF"
        PRINT #OutputFile, ""
        PRINT #OutputFile, "    vwatch64_FILEERROR:"
        PRINT #OutputFile, "    RESUME NEXT"
        PRINT #OutputFile, "$END IF"
    END IF
    PRINT #OutputFile,
    PRINT #OutputFile, "'$INCLUDE:'" + BMFileName + "'"
    CLOSE OutputFile
    CLOSE InputFile

    BIFile = FREEFILE
    OPEN BIFileName FOR OUTPUT AS #BIFile

    PRINT "Generating "; BIFileName; "..."
    'Creates a vWATCH64.BI customized for the .BAS provided:
    PRINT #BIFile, "$LET VWATCH64 = ON"
    PRINT #BIFile, ""
    PRINT #BIFile, "$IF VWATCH64 = ON THEN"
    $IF WIN THEN
        PRINT #BIFile, "        DECLARE LIBRARY"
        PRINT #BIFile, "            FUNCTION GetModuleFileNameA (BYVAL hModule AS LONG, lpFileName AS STRING, BYVAL nSize AS LONG)"
        PRINT #BIFile, "        END DECLARE"
    $END IF
    PRINT #BIFile, ""
    PRINT #BIFile, ""
    PRINT #BIFile, "    CONST vwatch64_ID = " + Q$ + "vWATCH64" + Q$
    PRINT #BIFile, "    CONST vwatch64_VERSION = " + Q$ + VERSION + Q$
    PRINT #BIFile, "    CONST vwatch64_INTERVAL = .1"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_HEADERTYPE"
    PRINT #BIFile, "        CLIENT_ID AS STRING * 8"
    PRINT #BIFile, "        VERSION AS STRING * 5"
    PRINT #BIFile, "        CONNECTED AS _BYTE"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_CLIENTTYPE"
    PRINT #BIFile, "        NAME AS STRING * 256"
    PRINT #BIFile, "        EXENAME AS STRING * 256"
    PRINT #BIFile, "        CURRENTMODULE AS STRING * 50"
    PRINT #BIFile, "        LASTOUTPUT AS DOUBLE"
    PRINT #BIFile, "        TOTALVARIABLES AS INTEGER"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_VARIABLESTYPE"
    PRINT #BIFile, "        NAME AS STRING * 40"
    PRINT #BIFile, "        SCOPE AS STRING * 7"
    PRINT #BIFile, "        UDT AS STRING * 40"
    PRINT #BIFile, "        DATATYPE AS STRING * 20"
    PRINT #BIFile, "        VALUE AS STRING * 255"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENTFILE AS INTEGER"
    PRINT #BIFile, "    DIM SHARED vwatch64_USERQUIT AS _BIT"
    PRINT #BIFile, "    DIM SHARED vwatch64_RESPONSE AS STRING * 6"
    PRINT #BIFile, "    DIM SHARED vwatch64_AUTHORIZED AS _BYTE"
    PRINT #BIFile, "    DIM SHARED vwatch64_CHECKPOINT AS INTEGER"
    PRINT #BIFile, "    DIM SHARED vwatch64_PREVLOF AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_LOF AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_TIMER AS INTEGER"
    PRINT #BIFile, "    DIM SHARED vwatch64_HEADER AS vwatch64_HEADERTYPE"
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENT AS vwatch64_CLIENTTYPE"
    PRINT #BIFile, "    DIM SHARED vwatch64_CLIENTDATA AS LONG"
    PRINT #BIFile, "    DIM SHARED vwatch64_DATABLOCK AS LONG"
    PRINT #BIFile, "    DIM vwatch64_EXENAME AS STRING * 256"
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_HEADER.CLIENT_ID = vwatch64_ID"
    PRINT #BIFile, "    vwatch64_HEADER.VERSION = vwatch64_VERSION"
    PRINT #BIFile, "    vwatch64_HEADER.CONNECTED = -1"
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_CLIENT.NAME = " + Q$ + NOPATH$(FILENAME$) + Q$
    PRINT #BIFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
    PRINT #BIFile, "    vwatch64_CLIENT.TOTALVARIABLES =" + STR$(TOTALVARIABLES)
    PRINT #BIFile, ""
    $IF WIN THEN
        PRINT #BIFile, "        Ret = GetModuleFileNameA(0, vwatch64_EXENAME, LEN(vwatch64_EXENAME))"
        PRINT #BIFile, "        IF Ret > 0 THEN"
        PRINT #BIFile, "            vwatch64_CLIENT.EXENAME = LEFT$(vwatch64_EXENAME, Ret)"
        PRINT #BIFile, "        END IF"
    $ELSE
        PRINT #BIFile, "        vwatch64_CLIENT.EXENAME = " + Q$ + Q$
    $END IF
    PRINT #BIFile, ""
    PRINT #BIFile, "    DIM SHARED vwatch64_VARIABLES(1 TO vwatch64_CLIENT.TOTALVARIABLES) AS vwatch64_VARIABLESTYPE"
    FOR i = 1 TO TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) THEN
            PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").NAME = " + Q$ + RTRIM$(VARIABLES(i).NAME) + Q$
            PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").SCOPE = " + Q$ + RTRIM$(VARIABLES(i).SCOPE) + Q$
            PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").DATATYPE = " + Q$ + RTRIM$(VARIABLES(i).DATATYPE) + Q$
        END IF
    NEXT i
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_CONNECTTOHOST"
    PRINT #BIFile, ""
    PRINT #BIFile, "    IF vwatch64_AUTHORIZED THEN"
    PRINT #BIFile, "        'Connection successful. Send client's ID:"
    PRINT #BIFile, "        vwatch64_CLIENTDATA = SEEK(vwatch64_CLIENTFILE)"
    PRINT #BIFile, "        PUT #vwatch64_CLIENTFILE, , vwatch64_CLIENT"
    PRINT #BIFile, ""
    PRINT #BIFile, "        vwatch64_DATABLOCK = SEEK(vwatch64_CLIENTFILE)"
    PRINT #BIFile, ""
    PRINT #BIFile, "        'Initialize the watch timer:"
    PRINT #BIFile, "        vwatch64_TIMER = _FREETIMER"
    PRINT #BIFile, "        ON TIMER(vwatch64_TIMER, vwatch64_INTERVAL) vwatch64_VARIABLEWATCH"
    PRINT #BIFile, "        TIMER(vwatch64_TIMER) ON"
    PRINT #BIFile, "    END IF"
    PRINT #BIFile, "$END IF"

    CLOSE BIFile

    BMFile = FREEFILE
    OPEN BMFileName FOR OUTPUT AS #BMFile

    PRINT "Generating "; BMFileName; "..."
    IF VERBOSE THEN _DELAY .05
    'Creates a vWATCH64.BM customized for the .BAS provided:
    PRINT #BMFile, "$IF VWATCH64 = ON THEN"
    PRINT #BMFile, "    SUB vwatch64_CONNECTTOHOST"
    PRINT #BMFile, "        vwatch64_CLIENTFILE = " + LTRIM$(RTRIM$(STR$(_CEIL(RND * 30000) + 100)))
    PRINT #BMFile, "        'You may be wondering why such a weird file number..."
    PRINT #BMFile, "        OPEN " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$ + " FOR BINARY AS vwatch64_CLIENTFILE"
    PRINT #BMFile, ""
    PRINT #BMFile, "        'Send this client's version"
    PRINT #BMFile, "        PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER"
    PRINT #BMFile, "        vwatch64_PREVLOF = LOF(vwatch64_CLIENTFILE)"
    PRINT #BMFile, ""
    PRINT #BMFile, "        'Wait for data to be sent by host:"
    PRINT #BMFile, "        vwatch64_WAITFORDATA"
    PRINT #BMFile, ""
    PRINT #BMFile, "        GET #vwatch64_CLIENTFILE, , vwatch64_RESPONSE"
    PRINT #BMFile, ""
    PRINT #BMFile, "        IF vwatch64_RESPONSE <> " + Q$ + "AUTHOK" + Q$ + " THEN"
    PRINT #BMFile, "            vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "            CLOSE #vwatch64_CLIENTFILE"
    PRINT #BMFile, "            ON ERROR GOTO vwatch64_FILEERROR"
    PRINT #BMFile, "            KILL " + Q$ + PATHONLY$(EXENAME) + "vwatch64.dat" + Q$
    PRINT #BMFile, "        ELSE"
    PRINT #BMFile, "            vwatch64_AUTHORIZED = -1"
    PRINT #BMFile, "        END IF"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, ""
    PRINT #BMFile, "    SUB vwatch64_VARIABLEWATCH"

    LocalSharedAddedTotal = 0
    FOR i = 1 TO TotalLocalVariables
        SourceLine = "    SHARED "
        found = FINDVARIABLES(TRIM$(LOCALVARIABLES(i).NAME))
        IF LEN(TRIM$(VARIABLES(found).UDT)) > 0 THEN
            IF TotalUDTsAdded > 0 THEN
                AlreadyAdded = 0
                FOR L = 1 TO TotalUDTsAdded
                    IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                        IF TRIM$(UDT_ADDED(L).UDT) = TRIM$(VARIABLES(found).UDT) AND LEFT$(VARIABLES(found).NAME, INSTR(VARIABLES(found).NAME, "(") - 1) = TRIM$(UDT_ADDED(L).NAME) THEN AlreadyAdded = -1
                    ELSE
                        IF TRIM$(UDT_ADDED(L).UDT) = TRIM$(VARIABLES(found).UDT) AND TRIM$(VARIABLES(found).NAME) = TRIM$(UDT_ADDED(L).NAME) THEN AlreadyAdded = -1
                    END IF
                NEXT L
                IF NOT AlreadyAdded THEN
                    'New local variable AS UDT found
                    TotalUDTsAdded = TotalUDTsAdded + 1
                    REDIM _PRESERVE UDT_ADDED(1 TO TotalUDTsAdded) AS VARIABLESTYPE
                    UDT_ADDED(TotalUDTsAdded).UDT = TRIM$(VARIABLES(found).UDT)
                    IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                        SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, "(")) + ") AS " + TRIM$(VARIABLES(found).UDT)
                        UDT_ADDED(TotalUDTsAdded).NAME = LEFT$(VARIABLES(found).NAME, INSTR(VARIABLES(found).NAME, "(") - 1)
                    ELSE
                        UDT_ADDED(TotalUDTsAdded).NAME = TRIM$(VARIABLES(found).NAME)
                        SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, ".") - 1) + " AS " + TRIM$(VARIABLES(found).UDT)
                    END IF
                    LocalSharedAddedTotal = LocalSharedAddedTotal + 1
                    REDIM _PRESERVE LOCALSHAREDADDED(1 TO LocalSharedAddedTotal) AS STRING
                    LOCALSHAREDADDED(LocalSharedAddedTotal) = SourceLine
                END IF
            ELSE
                'New local variable AS UDT found
                IF INSTR(LOCALVARIABLES(i).NAME, "(") THEN
                    SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, "(")) + ") AS " + TRIM$(VARIABLES(found).UDT)
                ELSE
                    SourceLine = SourceLine + LEFT$(TRIM$(LOCALVARIABLES(i).NAME), INSTR(LOCALVARIABLES(i).NAME, ".") - 1) + " AS " + TRIM$(VARIABLES(found).UDT)
                END IF
                TotalUDTsAdded = TotalUDTsAdded + 1
                REDIM _PRESERVE UDT_ADDED(1 TO TotalUDTsAdded) AS VARIABLESTYPE
                UDT_ADDED(TotalUDTsAdded).NAME = TRIM$(VARIABLES(found).NAME)
                UDT_ADDED(TotalUDTsAdded).UDT = TRIM$(VARIABLES(found).UDT)
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
    NEXT i

    LocalSharedNotRepeated = 0
    REDIM LocalShared_NOREPETITION(1 TO LocalSharedAddedTotal) AS STRING
    FOR i = 1 TO LocalSharedAddedTotal
        found = 0
        IF LocalSharedNotRepeated > 0 THEN
            FOR j = 1 TO LocalSharedNotRepeated
                IF LOCALSHAREDADDED(i) = LocalShared_NOREPETITION(j) THEN found = -1: EXIT FOR
            NEXT j
            IF NOT found THEN
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

    PRINT #BMFile, ""
    PRINT #BMFile, "        vwatch64_CLIENT.LASTOUTPUT = TIMER"
    PRINT #BMFile, "        PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTDATA, vwatch64_CLIENT"
    PRINT #BMFile, ""
    FOR i = 1 TO TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) THEN
            IF INSTR(VARIABLES(i).DATATYPE, "STRING") THEN
                SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").VALUE = " + RTRIM$(VARIABLES(i).NAME)
                PRINT #BMFile, SourceLine
            ELSE
                SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").VALUE = STR$(" + RTRIM$(VARIABLES(i).NAME) + ")"
                PRINT #BMFile, SourceLine
            END IF
        END IF
    NEXT i
    PRINT #BMFile, ""
    PRINT #BMFile, "        PUT #vwatch64_CLIENTFILE, vwatch64_DATABLOCK, vwatch64_VARIABLES()"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, ""
    PRINT #BMFile, "    SUB vwatch64_WAITFORDATA"
    PRINT #BMFile, "        vwatch64_WAITSTART# = TIMER"
    PRINT #BMFile, "        DO: _LIMIT 30"
    PRINT #BMFile, "            vwatch64_LOF = LOF(vwatch64_CLIENTFILE)"
    PRINT #BMFile, "            IF vwatch64_LOF > vwatch64_PREVLOF THEN"
    PRINT #BMFile, "                'Data received!"
    PRINT #BMFile, "                vwatch64_PREVLOF = vwatch64_LOF"
    PRINT #BMFile, "                EXIT DO"
    PRINT #BMFile, "            END IF"
    PRINT #BMFile, "            IF _KEYHIT = 27 THEN EXIT DO"
    PRINT #BMFile, "            IF TIMER - vwatch64_WAITSTART# > 3 THEN EXIT DO"
    PRINT #BMFile, "        LOOP"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, "$END IF"
    CLOSE BMFile
    PRINT "Done."
    SLEEP 1

    $IF WIN THEN
        ThisPath$ = ""
        ExecutableExtension$ = ".exe"
    $ELSE IF MAC OR LINUX THEN
        ThisPath$ = "./"
        ExecutableExtension$ = ""
    $END IF
    Compiler$ = "qb64" + ExecutableExtension$

    IF NOT DONTCOMPILE AND _FILEEXISTS(Compiler$) THEN
        PRINT "Attempting to compile...";
        IF SHELL(ThisPath$ + Compiler$ + " -c " + Q$ + NEWFILENAME$ + Q$) <> 0 THEN
            PRINT "failed."
            PRINT "Files have been generated, you will have to compile them yourself."
            PRINT "Press any key to go back..."
            SLEEP
            EXIT SUB
        ELSE
            PRINT "done."
            IF _FILEEXISTS(LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$) THEN
                SHELL _DONTWAIT LEFT$(NOPATH$(NEWFILENAME$), LEN(NOPATH$(NEWFILENAME$)) - 4) + ExecutableExtension$
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
FUNCTION FINDVARIABLES (Text$)
    FOR i = 1 TO TOTALVARIABLES
        IF TRIM$(VARIABLES(i).NAME) = TRIM$(Text$) THEN
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

    FOR i = 1 TO LEN(Text)
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
    DIM NewText$
    FOR i = 1 TO LEN(Text$)
        IF MID$(Text$, i, 1) = Char$ THEN EXIT FOR
        NewText$ = NewText$ + MID$(Text$, i, 1)
    NEXT i
    TRUNCATE$ = NewText$
END FUNCTION

'------------------------------------------------------------------------------
FUNCTION SUFFIXLOOKUP$ (Var AS STRING)
    IF LEN(Var) < 2 THEN EXIT FUNCTION

    SELECT CASE MID$(Var, LEN(Var), 1)
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
            FOR i = LEN(Var) - 1 TO 1 STEP -1
                SELECT CASE MID$(Var, i, 1)
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

    IF LEN(Var) < 3 THEN EXIT FUNCTION 'no more suffixes to evaluate

    SELECT CASE MID$(Var, LEN(Var) - 1, 1)
        CASE "~"
            SUFFIXLOOKUP$ = "_UNSIGNED " + SUFFIXLOOKUP$
        CASE "%"
            SUFFIXLOOKUP$ = "_BYTE"
        CASE "&"
            SUFFIXLOOKUP$ = "_INTEGER64"
        CASE "#"
            SUFFIXLOOKUP$ = "FLOAT"
    END SELECT

    IF LEN(Var) < 4 THEN EXIT FUNCTION 'no more suffixes to evaluate
    IF MID$(Var, LEN(Var) - 2, 1) = "~" THEN SUFFIXLOOKUP$ = "_UNSIGNED " + SUFFIXLOOKUP$
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

    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Waiting for a connection..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$
    t$ = "Launch the program that will be monitored now"
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT - _FONTHEIGHT * 2), t$
    t$ = "ESC to quit"
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT - _FONTHEIGHT), t$

    'Setup host header:
    HEADER.HOST_ID = ID
    HEADER.VERSION = VERSION
    HEADER.CONNECTED = 0

    FILE = FREEFILE
    CLOSE
    ON ERROR GOTO FileError
    'Try killing vwatch64.dat. Won't work if open, so we'll try to reconnect to client.
    KILL PATHONLY$(EXENAME) + "vwatch64.dat"

    'Opens "vwatch64.dat" to wait for a connection:
    OPEN PATHONLY$(EXENAME) + "vwatch64.dat" FOR BINARY AS #FILE

    'Wait for a connection:
    x = _EXIT
    SHOWMENU
    DO: _LIMIT 30
        GET #FILE, 1, HEADER
        MENU% = CHECKMENU(TRUE)
        IF MENU% = 101 THEN HIDEMENU: CLOSE: EXIT SUB
        k$ = INKEY$
        IF k$ = CHR$(27) THEN USERQUIT = -1
        IF _EXIT THEN USERQUIT = -1
    LOOP UNTIL USERQUIT OR HEADER.CONNECTED OR MENU% = 102
    HIDEMENU

    IF USERQUIT OR MENU% = 102 THEN
        CLOSE #FILE
        ON ERROR GOTO FileError
        KILL PATHONLY$(EXENAME) + "vwatch64.dat"
        SYSTEM
    END IF

    CLS
    'Connected! Check if client is compatible:
    IF HEADER.HOST_ID <> ID OR HEADER.VERSION <> VERSION THEN
        BEEP
        PRINT "Client not compatible."
        PRINT "Attempted connection by client with ID "; CHR$(34); HEADER.HOST_ID + CHR$(34)
        PRINT "Reported version: "; HEADER.VERSION
        PRINT "Press any key to go back..."
        'Report "DENIED" to the client:
        RESPONSE = "DENIED"
        PUT #FILE, , RESPONSE
        CLOSE #FILE
        SLEEP
        EXIT SUB
    END IF

    'Send autorization to client:
    RESPONSE = "AUTHOK"
    PUT #FILE, , RESPONSE
    PREVLOF = LOF(FILE)

    CLS
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Connected. Waiting for client ID..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$

    CLIENTDATA = SEEK(FILE)

    'LEN(HEADER) + RESPONSE bytes have been transmitted so far. If LOF() is bigger than that,
    'we are connecting to an active previously connected client.
    IF PREVLOF <= LEN(HEADER) + LEN(RESPONSE) THEN
        'Wait for data to be sent by client:
        WAITFORDATA
    END IF

    GET #FILE, , CLIENT
    DATABLOCK = SEEK(FILE)

    TITLESTRING = TITLESTRING + " - " + TRIM$(CLIENT.NAME) + IIFSTR$(LEN(TRIM$(CLIENT.EXENAME)), " (" + TRIM$(CLIENT.EXENAME) + ")", "")
    _TITLE TITLESTRING
END SUB

'------------------------------------------------------------------------------
SUB MONITOR_MODE
    INFOSCREENHEIGHT = _FONTHEIGHT * (CLIENT.TOTALVARIABLES + 6)
    IF INFOSCREENHEIGHT > 600 THEN
        INFOSCREEN = _NEWIMAGE(1000, INFOSCREENHEIGHT, 32)
        SB_Ratio = _HEIGHT(MAINSCREEN) / INFOSCREENHEIGHT
        SB_ThumbH = (_HEIGHT(MAINSCREEN) * SB_Ratio) - 6
        _DEST INFOSCREEN
        CLS
        $IF WIN THEN
            IF TTFONT THEN _FONT TTFONT
        $END IF
    END IF

    COLOR _RGB32(0, 0, 0), _RGBA32(0, 0, 0, 0)
    CLS

    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(ID) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), ID
    t$ = "Waiting for variable stream..."
    _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(t$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), t$

    WAITFORDATA

    filter$ = ""
    searchIn = VARIABLENAMES
    SB_ThumbY = 0
    t$ = "(end of list)"

    longestVarName = 1
    GET #FILE, DATABLOCK, VARIABLES()
    FOR i = 1 TO CLIENT.TOTALVARIABLES
        IF LEN(TRIM$(VARIABLES(i).NAME)) > longestVarName THEN longestVarName = LEN(TRIM$(VARIABLES(i).NAME))
    NEXT i

    StartOfLoop# = TIMER
    DO: _LIMIT 60
        k$ = INKEY$
        IF LEN(k$) THEN k = ASC(k$) ELSE k = 0
        SELECT CASE k
            CASE 32 TO 126 'Printable ASCII characters
                'CASE 48 TO 57, 65 TO 90, 97 TO 122, ASC("."), ASC("_") 'Numbers, letters, period and underscore
                filter$ = filter$ + CHR$(k)
                y = 0
            CASE 8 'Backspace
                IF LEN(filter$) THEN filter$ = LEFT$(filter$, LEN(filter$) - 1)
                y = 0
            CASE 22 'CTRL + V
                IF LEN(_CLIPBOARD$) THEN filter$ = filter$ + _CLIPBOARD$: y = 0
            CASE 9 'TAB alternates between what is filtered (VARIABLENAMES, DATATYPES, VALUES)
                searchIn = (searchIn) MOD 3 + 1
                y = 0
            CASE 27 'ESC clears the current search filter or exits the program
                IF LEN(filter$) THEN
                    filter$ = ""
                ELSE
                    USERQUIT = -1: EXIT DO
                END IF
        END SELECT

        IF INFOSCREENHEIGHT > 600 THEN
            DO
                y = y + (_MOUSEWHEEL * (_HEIGHT(MAINSCREEN) / 5))
                mx = _MOUSEX
                my = _MOUSEY
                mb = _MOUSEBUTTON(1)
            LOOP WHILE _MOUSEINPUT

            IF mb THEN
                IF mx > _WIDTH(MAINSCREEN) - 30 AND mx < _WIDTH(MAINSCREEN) THEN
                    'Clicked inside the scroll bar. Check if click was on the thumb:
                    IF my > SB_ThumbY AND my < SB_ThumbY + SB_ThumbH THEN
                        'Clicked on the thumb:
                        grabbedY = my: starty = y
                        DO WHILE _MOUSEBUTTON(1)
                            m = _MOUSEINPUT
                            my = _MOUSEY
                            y = starty + ((my - grabbedY) / SB_Ratio)
                            GOSUB displaypic
                        LOOP
                    ELSE
                        'Clicked above or below the thumb:
                        IF my < SB_ThumbY THEN
                            m = _MOUSEINPUT
                            y = y - (_HEIGHT(PIC) * SB_Ratio)
                        ELSE
                            m = _MOUSEINPUT
                            y = y + ((_HEIGHT(PIC) - y) * SB_Ratio)
                        END IF
                    END IF
                END IF
            END IF

            IF _KEYDOWN(18432) THEN y = y - _FONTHEIGHT
            IF _KEYDOWN(20480) THEN y = y + _FONTHEIGHT
        ELSE
            IF INFOSCREEN < -1 THEN y = 0: GOSUB displaypic
        END IF

        CLS , _RGB32(255, 255, 255)
        GET #FILE, 1, HEADER
        GET #FILE, CLIENTDATA, CLIENT
        GET #FILE, DATABLOCK, VARIABLES()

        cursorBlink% = cursorBlink% + 1
        IF cursorBlink% > 30 THEN cursorBlink% = 0

        LINE (0, 0)-(_WIDTH(MAINSCREEN), 50), _RGB32(102, 255, 102), BF
        _PRINTSTRING (5, 3), "Now running: " + RTRIM$(CLIENT.CURRENTMODULE)
        _PRINTSTRING (5, _FONTHEIGHT + 3), "Total variables:" + STR$(CLIENT.TOTALVARIABLES)
        _PRINTSTRING (5, _FONTHEIGHT * 2 + 3), IIFSTR$(LEN(filter$), "Filter " + IIFSTR$(searchIn = VARIABLENAMES, "(variable names): ", IIFSTR$(searchIn = DATATYPES, "(data types)    : ", "(values)        : ")) + UCASE$(filter$) + IIFSTR$(cursorBlink% > 15, CHR$(179), ""), "Start typing to filter " + IIFSTR$(searchIn = VARIABLENAMES, "variable names (TAB to change fields)", IIFSTR$(searchIn = DATATYPES, "data types (TAB to change fields)", "values (TAB to change fields)")))

        IF CLIENT.LASTOUTPUT > 0 THEN
            IF TIMER - CLIENT.LASTOUTPUT > 5 THEN TIMEOUT = -1
        ELSE
            IF TIMER - StartOfLoop# > TIMEOUTLIMIT THEN TIMEOUT = -1
        END IF

        'Places a light gray rectangle under the column that can currently be filtered
        SELECT CASE searchIn
            CASE DATATYPES
                columnHighlightX = _PRINTWIDTH(SPACE$(7))
                columnHighlightY = 55
                columnHighlightW = _PRINTWIDTH(SPACE$(20)) + 8
            CASE VARIABLENAMES
                columnHighlightX = _PRINTWIDTH(SPACE$(21)) + _PRINTWIDTH(SPACE$(7))
                columnHighlightY = 55
                columnHighlightW = _PRINTWIDTH(SPACE$(longestVarName)) + 8
            CASE VALUES
                columnHighlightX = _PRINTWIDTH(SPACE$(longestVarName)) + _PRINTWIDTH(SPACE$(20)) + _PRINTWIDTH(SPACE$(7)) + 16
                columnHighlightY = 55
                columnHighlightW = _WIDTH
        END SELECT
        LINE (columnHighlightX, columnHighlightY)-STEP(columnHighlightW, IIF(LEN(filter$), row, CLIENT.TOTALVARIABLES) * _FONTHEIGHT + 8), _RGB32(230, 230, 230), BF

        'Update list:
        i = 0: row = 0
        DO
            i = i + 1
            IF i > CLIENT.TOTALVARIABLES THEN EXIT DO
            IF LEN(filter$) THEN
                Found = 0
                SELECT CASE searchIn
                    CASE VARIABLENAMES: IF INSTR(UCASE$(VARIABLES(i).NAME), UCASE$(filter$)) THEN Found = -1
                    CASE DATATYPES: IF INSTR(VARIABLES(i).DATATYPE, UCASE$(filter$)) THEN Found = -1
                    CASE VALUES: IF INSTR(UCASE$(VARIABLES(i).VALUE), UCASE$(filter$)) THEN Found = -1
                END SELECT
                IF Found THEN
                    row = row + 1
                    v$ = VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + RTRIM$(VARIABLES(i).VALUE)
                    _PRINTSTRING (5, (3 + row) * _FONTHEIGHT), v$
                END IF
            ELSE
                INFOSCREENHEIGHT = _FONTHEIGHT * (CLIENT.TOTALVARIABLES + 6)
                v$ = VARIABLES(i).SCOPE + VARIABLES(i).DATATYPE + " " + LEFT$(VARIABLES(i).NAME, longestVarName) + " = " + RTRIM$(VARIABLES(i).VALUE)
                _PRINTSTRING (5, (3 + i) * _FONTHEIGHT), v$
            END IF
        LOOP

        IF LEN(filter$) AND row = 0 THEN 'A filter is on, but nothing was found
            _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT), "Not found."
            _PRINTSTRING (columnHighlightX + 5, 4 * _FONTHEIGHT + _FONTHEIGHT), "(ESC to clear)"
        END IF

        IF INFOSCREENHEIGHT > 600 THEN
            IF LEN(filter$) AND row > 0 THEN
                _PRINTSTRING (5, (5 + row) * _FONTHEIGHT), t$ + "(filtered)"
            ELSEIF LEN(filter$) = 0 THEN
                _PRINTSTRING (5, _HEIGHT(INFOSCREEN) - _FONTHEIGHT), t$
            END IF
            GOSUB displaypic
        ELSE
            'End of list message:
            IF LEN(filter$) AND row > 0 THEN
                _PRINTSTRING (5, (5 + row) * _FONTHEIGHT), t$ + "(filtered)"
            ELSEIF LEN(filter$) = 0 THEN
                _PRINTSTRING (5, (4 + i) * _FONTHEIGHT), t$
            END IF
        END IF
        _DISPLAY
        IF _EXIT THEN USERQUIT = -1: EXIT DO
    LOOP UNTIL HEADER.CONNECTED = 0 OR TIMEOUT

    IF INFOSCREENHEIGHT > 600 THEN _DEST MAINSCREEN
    _AUTODISPLAY

    OVERLAYSCREEN = _NEWIMAGE(500, 300, 32)
    _DEST OVERLAYSCREEN
    _FONT 16
    LINE (0, 0)-STEP(799, 599), _RGBA32(255, 255, 255, 200), BF

    IF HEADER.CONNECTED = 0 THEN
        EndMessage$ = "Connection closed by client."
    ELSEIF TIMEOUT THEN
        EndMessage$ = "Connection timed out."
    END IF

    CLOSE
    ON ERROR GOTO FileError
    KILL PATHONLY$(EXENAME) + "vwatch64.dat"

    IF HEADER.CONNECTED = 0 OR TIMEOUT THEN
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
        DO: _LIMIT 30
            IF _EXIT THEN USERQUIT = -1: EXIT DO
        LOOP UNTIL _KEYHIT
    END IF

    EXIT SUB
    displaypic:
    IF y < 0 THEN y = 0
    IF INFOSCREENHEIGHT > 600 THEN
        IF y > INFOSCREENHEIGHT - _HEIGHT(MAINSCREEN) THEN y = INFOSCREENHEIGHT - _HEIGHT(MAINSCREEN)
    ELSE
        y = 0
    END IF
    _PUTIMAGE (0, 0)-STEP(_WIDTH(MAINSCREEN) - 1, _HEIGHT(MAINSCREEN) - 1), INFOSCREEN, MAINSCREEN, (0, y)-STEP(_WIDTH(MAINSCREEN) - 1, _HEIGHT(MAINSCREEN) - 1)

    ShowScroll = 1
    IF LEN(filter$) AND row > 0 THEN
        INFOSCREENHEIGHT = _FONTHEIGHT * (row + 6)
        IF INFOSCREENHEIGHT < 600 THEN
            ShowScroll = 0
        ELSE
            SB_Ratio = _HEIGHT(MAINSCREEN) / INFOSCREENHEIGHT
            SB_ThumbH = (_HEIGHT(MAINSCREEN) * SB_Ratio)
        END IF
    ELSE
        INFOSCREENHEIGHT = _FONTHEIGHT * (CLIENT.TOTALVARIABLES + 6)
        SB_Ratio = _HEIGHT(MAINSCREEN) / INFOSCREENHEIGHT
        SB_ThumbH = (_HEIGHT(MAINSCREEN) * SB_Ratio)
    END IF
    'Scrollbar:
    IF ShowScroll THEN
        _DEST MAINSCREEN
        SB_ThumbY = (y * SB_Ratio)
        LINE (_WIDTH(MAINSCREEN) - 30, 0)-STEP(29, _HEIGHT(MAINSCREEN) - 1), _RGB32(170, 170, 170), BF
        LINE (_WIDTH(MAINSCREEN) - 25, SB_ThumbY + 3)-STEP(19, SB_ThumbH - 7), _RGB32(70, 70, 70), BF
        _DEST INFOSCREEN
    END IF
    _DISPLAY
    RETURN

END SUB

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
            LowerBoundary% = OPTIONBASE
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

FUNCTION SelectFile$ (search$, x AS INTEGER, y AS INTEGER)
    'save some old values
    LoadFile_DC = _DEFAULTCOLOR: LoadFile_BG = _BACKGROUNDCOLOR
    LoadFile_s = _SOURCE: LoadFile_d = _DEST
    f = _FONT: _FONT 16
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
                                LoadFile_DirList(i, LoadFile_Count(i)) = LEFT$(LoadFile_nam$, LEN(LoadFile_nam$) - LEN(LoadFile_Label(i)))
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
            IF LoadFile_Label(row) <> ".*" AND LoadFile_Label(row) <> "DIR" THEN temp$ = LoadFile_DirList(row, selection) + LoadFile_Label(row) ELSE temp$ = LoadFile_DirList(row, selection)
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
                    _FONT f
                    EXIT SUB 'And leave
                CASE 27 'If ESC is pressed then...
                    DO: LOOP UNTIL INKEY$ = "" 'Clear the keyboard buffer so it doesn't affect the main program.
                    COLOR LoadFile_DC, LoadFile_BG: _SOURCE LoadFile_s: _DEST LoadFile_d: PCOPY 1, 0: _DISPLAY: SelectFile$ = "" 'Restore our old settings
                    _FONT f
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
                _FONT f
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
            IF LoadFile_Label(row) <> ".*" AND LoadFile_Label(row) <> "DIR" THEN temp1$ = LoadFile_DirList(row, selection) + LoadFile_Label(row) ELSE temp1$ = LoadFile_DirList(row, selection)
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
    _FONT f
END SUB

