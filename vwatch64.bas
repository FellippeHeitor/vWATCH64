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

CONST vwatch64_ID = "vWATCH64"
CONST vwatch64_VERSION = "0.1b "

TYPE vwatch64_HEADERTYPE
    HOST_ID AS STRING * 8
    VERSION AS STRING * 5
    CONNECTED AS _BYTE
END TYPE

TYPE vwatch64_CLIENTTYPE
    NAME AS STRING * 255
    CURRENTMODULE AS STRING * 255
    TOTALVARIABLES AS INTEGER
END TYPE

TYPE vwatch64_VARIABLESTYPE
    NAME AS STRING * 128
    DATATYPE AS STRING * 20
    VALUE AS STRING * 255
END TYPE

DIM SHARED vwatch64_MS AS LONG
DIM SHARED vwatch64_INFOSCREEN AS LONG
DIM SHARED vwatch64_INFOSCREENHEIGHT AS INTEGER
DIM SHARED vwatch64_FILE AS INTEGER
DIM SHARED vwatch64_USERQUIT AS _BIT
DIM SHARED vwatch64_RESPONSE AS STRING * 6
DIM SHARED vwatch64_PREVLOF AS LONG
DIM SHARED vwatch64_LOF AS LONG
DIM SHARED vwatch64_DATABLOCK AS LONG
DIM SHARED vwatch64_CLIENTDATA AS LONG
DIM SHARED vwatch64_HEADER AS vwatch64_HEADERTYPE
DIM SHARED vwatch64_CLIENT AS vwatch64_CLIENTTYPE
DIM SHARED vwatch64_TITLESTRING AS STRING
DIM i AS INTEGER

'Did the user drag a .BAS file onto this program?
IF LEN(COMMAND$) THEN
    IF _FILEEXISTS(COMMAND$) THEN
        vwatch64_PROCESSFILE COMMAND$
    END IF
END IF

'Screen setup:
vwatch64_MS = _NEWIMAGE(800, 600, 12)
SCREEN vwatch64_MS
COLOR 0, 15
CLS
_FONT 16
_TITLE "vWATCH64"
_SCREENSHOW

PRINT "vWATCH64"
WaitForConnection:
PRINT "Waiting for a connection..."

'Setup host header:
vwatch64_HEADER.HOST_ID = vwatch64_ID
vwatch64_HEADER.VERSION = vwatch64_VERSION
vwatch64_HEADER.CONNECTED = 0

vwatch64_FILE = FREEFILE

'Erases the contents of any existing "vwatch64.dat":
OPEN "vwatch64.dat" FOR OUTPUT AS #vwatch64_FILE
CLOSE #vwatch64_FILE

'Creates a new "vwatch64.dat" to wait for a connection:
OPEN "vwatch64.dat" FOR BINARY AS #vwatch64_FILE

'Wait for a connection:
x = _EXIT
DO: _LIMIT 10
    GET #vwatch64_FILE, 1, vwatch64_HEADER
    IF vwatch64_READKEYBOARD = 27 THEN vwatch64_USERQUIT = -1
    IF _EXIT THEN vwatch64_USERQUIT = -1
LOOP UNTIL vwatch64_USERQUIT OR vwatch64_HEADER.CONNECTED

IF vwatch64_USERQUIT THEN
    CLOSE #vwatch64_FILE
    KILL "vwatch64.dat"
    SYSTEM
END IF

'Connected! Check if client is compatible:
IF vwatch64_HEADER.HOST_ID <> vwatch64_ID OR vwatch64_HEADER.VERSION <> vwatch64_VERSION THEN
    PRINT "Client not compatible."
    PRINT "Attempted connection by client with ID "; CHR$(34); vwatch64_HEADER.HOST_ID + CHR$(34)
    PRINT "Reported version: "; vwatch64_HEADER.VERSION
    'Report "DENIED" to the client:
    vwatch64_RESPONSE = "DENIED"
    PUT #vwatch64_FILE, , vwatch64_RESPONSE
    CLOSE #vwatch64_FILE
    END
END IF

'Send autorization to client:
vwatch64_RESPONSE = "AUTHOK"
PUT #vwatch64_FILE, , vwatch64_RESPONSE
vwatch64_PREVLOF = LOF(vwatch64_FILE)

PRINT "Connected. Waiting for client ID..."

vwatch64_CLIENTDATA = SEEK(vwatch64_FILE)
'Wait for data to be sent by client:
vwatch64_WAITFORDATA
GET #vwatch64_FILE, , vwatch64_CLIENT
vwatch64_DATABLOCK = SEEK(vwatch64_FILE)

vwatch64_TITLESTRING = "Source: " + RTRIM$(vwatch64_CLIENT.NAME)
_TITLE vwatch64_TITLESTRING

REDIM SHARED vwatch64_VARIABLES(1 TO vwatch64_CLIENT.TOTALVARIABLES) AS vwatch64_VARIABLESTYPE
vwatch64_INFOSCREENHEIGHT = _FONTHEIGHT * (vwatch64_CLIENT.TOTALVARIABLES + 7)
IF vwatch64_INFOSCREENHEIGHT > 600 THEN
    vwatch64_INFOSCREEN = _NEWIMAGE(800, vwatch64_INFOSCREENHEIGHT, 12)
    _DEST vwatch64_INFOSCREEN
    CLS
    COLOR 0, 15
END IF

t$ = "(end of list)"
DO: _LIMIT 30
    CLS
    IF vwatch64_INFOSCREENHEIGHT > 600 THEN
        DO WHILE _MOUSEINPUT
            mw = _MOUSEWHEEL
            y = y + mw * 10
        LOOP
        IF _KEYDOWN(18432) THEN y = y - 10
        IF _KEYDOWN(20480) THEN y = y + 10
        IF y < 0 THEN y = 0
        IF y > _HEIGHT(vwatch64_INFOSCREEN) - 599 THEN y = _HEIGHT(vwatch64_INFOSCREEN) - 599
        _PRINTSTRING (_WIDTH(vwatch64_MS) / 2 - (_PRINTWIDTH(t$) / 1), _HEIGHT(vwatch64_INFOSCREEN) - _FONTHEIGHT), t$
    END IF
    GET #vwatch64_FILE, vwatch64_CLIENTDATA, vwatch64_CLIENT

    PRINT vwatch64_TITLESTRING
    PRINT "Running:         "; RTRIM$(vwatch64_CLIENT.CURRENTMODULE)
    PRINT "Total variables: "; vwatch64_CLIENT.TOTALVARIABLES

    GET #vwatch64_FILE, 1, vwatch64_HEADER
    GET #vwatch64_FILE, vwatch64_DATABLOCK, vwatch64_VARIABLES()

    FOR i = 1 TO vwatch64_CLIENT.TOTALVARIABLES
        PRINT vwatch64_VARIABLES(i).DATATYPE, RTRIM$(vwatch64_VARIABLES(i).NAME); " = "; RTRIM$(vwatch64_VARIABLES(i).VALUE)
    NEXT i
    IF vwatch64_INFOSCREENHEIGHT > 600 THEN
        _PUTIMAGE (0, 0)-STEP(_WIDTH(vwatch64_MS) - 1, _HEIGHT(vwatch64_MS) - 1), vwatch64_INFOSCREEN, vwatch64_MS, (0, y)-STEP(_WIDTH(vwatch64_MS) - 1, _HEIGHT(vwatch64_MS) - 1)
        'Scrollbar:
        _DEST vwatch64_MS
        LINE (_WIDTH(vwatch64_MS) - 30, 0)-STEP(29, _HEIGHT(vwatch64_MS) - 1), 7, BF
        LINE (_WIDTH(vwatch64_MS) - 25, y)-STEP(19, ((_HEIGHT(vwatch64_MS) - 1) / vwatch64_INFOSCREENHEIGHT) * (_HEIGHT(vwatch64_MS) - (5 * _FONTHEIGHT))), 8, BF
        _DEST vwatch64_INFOSCREEN
    ELSE
        _PRINTSTRING (_WIDTH(vwatch64_MS) / 2 - (_PRINTWIDTH(t$) / 1), CSRLIN * _FONTHEIGHT), t$
    END IF
    _DISPLAY
    IF _EXIT THEN EXIT DO
LOOP UNTIL vwatch64_READKEYBOARD = 27 OR vwatch64_HEADER.CONNECTED = 0
IF vwatch64_INFOSCREENHEIGHT > 600 THEN _DEST vwatch64_MS
_AUTODISPLAY

IF vwatch64_HEADER.CONNECTED = 0 THEN
    PRINT "Connection closed by client. Press any key to exit..."
    SLEEP
END IF
SYSTEM

KeyWordsDATA:
DATA _BIT,_UNSIGNED _BIT,_BYTE,_UNSIGNED _BYTE,INTEGER
DATA _UNSIGNED INTEGER,LONG,_UNSIGNED LONG,_INTEGER64
DATA _UNSIGNED _INTEGER64,SINGLE,DOUBLE,_FLOAT,STRING
DATA END

FUNCTION vwatch64_READKEYBOARD
    vwatch64_READKEYBOARD = _KEYHIT
END FUNCTION

SUB vwatch64_WAITFORDATA
    DO: _LIMIT 30
        vwatch64_LOF = LOF(vwatch64_FILE)
        IF vwatch64_LOF > vwatch64_PREVLOF THEN
            'Data received!
            vwatch64_PREVLOF = vwatch64_LOF
            EXIT DO
        END IF
        IF vwatch64_READKEYBOARD = 27 THEN EXIT DO
    LOOP
END SUB

SUB vwatch64_PROCESSFILE (FILENAME$)
    DIM InputFile AS INTEGER
    DIM OutputFile AS INTEGER
    DIM BIFile AS INTEGER
    DIM BIFileName AS STRING
    DIM BMFile AS INTEGER
    DIM BMFileName AS STRING
    DIM SourceLine AS STRING
    DIM TotalVariables AS INTEGER
    DIM ThisVariableName AS STRING
    DIM ThisVariableType AS STRING
    DIM TotalKeywords AS INTEGER
    DIM ThisKeyword AS STRING

    REDIM vwatch64_VARIABLES(1) AS vwatch64_VARIABLESTYPE

    RESTORE KeyWordsDATA
    DO
        READ ThisKeyword
        IF ThisKeyword = "END" THEN EXIT DO
        TotalKeywords = TotalKeywords + 1
        REDIM _PRESERVE KeywordList(1 TO TotalKeywords) AS STRING
        KeywordList(TotalKeywords) = ThisKeyword
    LOOP


    'Parses a .BAS file and reads all shared variable names
    'in order to generate a compatible vWATCH64 client.

    Q$ = CHR$(34)

    PRINT "Processing file: "; FILENAME$
    INPUT "New file name (without .BAS - will be replaced if already exists): ", NEWFILENAME$
    IF LEN(NEWFILENAME$) = 0 THEN BEEP: CLOSE: SYSTEM

    BIFileName = NEWFILENAME$ + ".BI"
    BMFileName = NEWFILENAME$ + ".BM"
    NEWFILENAME$ = NEWFILENAME$ + ".BAS"

    InputFile = FREEFILE
    OPEN FILENAME$ FOR INPUT AS #InputFile

    OutputFile = FREEFILE
    OPEN NEWFILENAME$ FOR OUTPUT AS #OutputFile

    BIFile = FREEFILE
    OPEN BIFileName FOR OUTPUT AS #BIFile

    BMFile = FREEFILE
    OPEN BMFileName FOR OUTPUT AS #BMFile


    'Injects the required code into processed file:
    PRINT #OutputFile, "'$INCLUDE:'" + BIFileName + "'"

    'Looks for DIM SHARED variables inside the main module and stores that
    'information in vwatch64_VARIABLES(). If SUB or FUNCTION is found,
    'injects CURRENTMODULE verification code. If SYSTEM is found, injects
    'cleanup procedures.
    TotalVariables = 0
    PRINT "Parsing .BAS..."
    DO
        LINE INPUT #InputFile, SourceLine
        SourceLine = RTRIM$(LTRIM$(SourceLine))
        IF LEFT$(SourceLine, 11) = "DIM SHARED " THEN
            'If it's not an array, we'll process this variable.
            IF INSTR(SourceLine, "(") = 0 AND vwatch64_CHECKKEYWORDS(RIGHT$(SourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3), KeywordList()) THEN
                TotalVariables = TotalVariables + 1
                REDIM _PRESERVE vwatch64_VARIABLES(1 TO TotalVariables) AS vwatch64_VARIABLESTYPE
                vwatch64_VARIABLES(TotalVariables).NAME = MID$(SourceLine, 12, INSTR(SourceLine, " AS ") - 12)
                vwatch64_VARIABLES(TotalVariables).DATATYPE = RIGHT$(SourceLine, LEN(SourceLine) - INSTR(SourceLine, " AS ") - 3)
                PRINT "Found "; TotalVariables;
                PRINT vwatch64_VARIABLES(TotalVariables).DATATYPE,
                PRINT RTRIM$(vwatch64_VARIABLES(TotalVariables).NAME)
                PRINT #OutputFile, SourceLine
            ELSE
                PRINT #OutputFile, SourceLine
            END IF
        ELSEIF LEFT$(SourceLine, 4) = "SUB " THEN
            PRINT #OutputFile, SourceLine
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            IF INSTR(SourceLine, "(") THEN
                PRINT "SUB "; MID$(SourceLine, 5, INSTR(SourceLine, "(") - 5)
                SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "SUB " + MID$(SourceLine, 5, INSTR(SourceLine, "(") - 5) + Q$
            ELSE
                PRINT SourceLine
                SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + SourceLine + Q$
            END IF
            PRINT #OutputFile, SourceLine
            PRINT #OutputFile, "$END IF"
        ELSEIF LEFT$(SourceLine, 9) = "FUNCTION " THEN
            PRINT #OutputFile, SourceLine
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            IF INSTR(SourceLine, "(") THEN
                PRINT "FUNCTION "; MID$(SourceLine, 10, INSTR(SourceLine, "(") - 10)
                SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "FUNCTION " + MID$(SourceLine, 10, INSTR(SourceLine, "(") - 10) + Q$
            ELSE
                PRINT SourceLine
                SourceLine = "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + SourceLine + Q$
            END IF
            PRINT #OutputFile, SourceLine
            PRINT #OutputFile, "$END IF"
        ELSEIF LEFT$(SourceLine, 7) = "END SUB" OR LEFT$(SourceLine, 12) = "END FUNCTION" THEN
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE"
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile, SourceLine
        ELSEIF INSTR(SourceLine, "EXIT SUB") OR INSTR(SourceLine, "EXIT FUNCTION") THEN
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE"
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile, SourceLine
        ELSEIF SourceLine = "SYSTEM" THEN
            PRINT #OutputFile, "$IF VWATCH64 = ON THEN"
            PRINT #OutputFile, "    IF vwatch64_AUTHORIZED THEN"
            PRINT #OutputFile, "        vwatch64_HEADER.CONNECTED = 0"
            PRINT #OutputFile, "        PUT #vwatch64_FILE, 1, vwatch64_HEADER"
            PRINT #OutputFile, "        CLOSE #vwatch64_FILE"
            PRINT #OutputFile, "    END IF"
            PRINT #OutputFile, "$END IF"
            PRINT #OutputFile, SourceLine
        ELSE
            PRINT #OutputFile, SourceLine
        END IF
    LOOP UNTIL EOF(InputFile)

    IF TotalVariables = 0 THEN
        BEEP
        PRINT "There are no SHARED variables in the .BAS you provided."
        CLOSE
        KILL NEWFILENAME$
        KILL BMFileName
        KILL BIFileName
        SLEEP 1
        END
    ELSE PRINT "Total SHARED variables found: "; TotalVariables
    END IF

    PRINT #OutputFile, "'$INCLUDE:'" + BMFileName + "'"
    CLOSE OutputFile
    CLOSE InputFile

    PRINT "Generating "; BIFileName; "..."
    'Creates a vWATCH64.BI customized for the .BAS provided:
    PRINT #BIFile, "$LET VWATCH64 = ON"
    PRINT #BIFile, ""
    PRINT #BIFile, "$IF VWATCH64 = ON THEN"
    PRINT #BIFile, "    CONST vwatch64_ID = " + Q$ + "vWATCH64" + Q$
    PRINT #BIFile, "    CONST vwatch64_VERSION = " + Q$ + "0.1b " + Q$
    PRINT #BIFile, "    CONST vwatch64_INTERVAL = .1"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_HEADERTYPE"
    PRINT #BIFile, "        CLIENT_ID AS STRING * 8"
    PRINT #BIFile, "        VERSION AS STRING * 5"
    PRINT #BIFile, "        CONNECTED AS _BYTE"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_CLIENTTYPE"
    PRINT #BIFile, "        NAME AS STRING * 255"
    PRINT #BIFile, "        CURRENTMODULE AS STRING * 255"
    PRINT #BIFile, "        TOTALVARIABLES AS INTEGER"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    TYPE vwatch64_VARIABLESTYPE"
    PRINT #BIFile, "        NAME AS STRING * 128"
    PRINT #BIFile, "        DATATYPE AS STRING * 20"
    PRINT #BIFile, "        VALUE AS STRING * 255"
    PRINT #BIFile, "    END TYPE"
    PRINT #BIFile, ""
    PRINT #BIFile, "    DIM SHARED vwatch64_FILE AS INTEGER"
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
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_HEADER.CLIENT_ID = vwatch64_ID"
    PRINT #BIFile, "    vwatch64_HEADER.VERSION = vwatch64_VERSION"
    PRINT #BIFile, "    vwatch64_HEADER.CONNECTED = -1"
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_CLIENT.NAME = " + Q$ + FILENAME$ + Q$
    PRINT #BIFile, "    vwatch64_CLIENT.CURRENTMODULE = " + Q$ + "MAIN MODULE" + Q$
    PRINT #BIFile, "    vwatch64_CLIENT.TOTALVARIABLES =" + STR$(TotalVariables)
    PRINT #BIFile, ""
    PRINT #BIFile, "    DIM SHARED vwatch64_VARIABLES(1 TO vwatch64_CLIENT.TOTALVARIABLES) AS vwatch64_VARIABLESTYPE"
    FOR i = 1 TO TotalVariables
        PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").NAME = " + Q$ + RTRIM$(vwatch64_VARIABLES(i).NAME) + Q$
        PRINT #BIFile, "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").DATATYPE = " + Q$ + RTRIM$(vwatch64_VARIABLES(i).DATATYPE) + Q$
    NEXT i
    PRINT #BIFile, ""
    PRINT #BIFile, "    vwatch64_CONNECTTOHOST"
    PRINT #BIFile, ""
    PRINT #BIFile, "    IF vwatch64_AUTHORIZED THEN"
    PRINT #BIFile, "        'Connection successful. Send client's ID:"
    PRINT #BIFile, "        vwatch64_CLIENTDATA = SEEK(vwatch64_FILE)"
    PRINT #BIFile, "        PUT #vwatch64_FILE, , vwatch64_CLIENT"
    PRINT #BIFile, ""
    PRINT #BIFile, "        vwatch64_DATABLOCK = SEEK(vwatch64_FILE)"
    PRINT #BIFile, ""
    PRINT #BIFile, "        'Initialize the watch timer:"
    PRINT #BIFile, "        ON TIMER(vwatch64_TIMER, vwatch64_INTERVAL) vwatch64_VARIABLEWATCH"
    PRINT #BIFile, "        TIMER(vwatch64_TIMER) ON"
    PRINT #BIFile, "    END IF"
    PRINT #BIFile, "$END IF"

    CLOSE BIFile

    PRINT "Generating "; BMFileName; "..."
    'Creates a vWATCH64.BM customized for the .BAS provided:
    PRINT #BMFile, "$IF VWATCH64 = ON THEN"
    PRINT #BMFile, "    SUB vwatch64_CONNECTTOHOST"
    PRINT #BMFile, "        IF _FILEEXISTS(" + Q$ + "vwatch64.dat" + Q$ + ") THEN"
    PRINT #BMFile, "            vwatch64_FILE = FREEFILE"
    PRINT #BMFile, "            OPEN " + Q$ + "vwatch64.dat" + Q$ + " FOR BINARY AS vwatch64_FILE"
    PRINT #BMFile, ""
    PRINT #BMFile, "            'Send this client's version"
    PRINT #BMFile, "            PUT #vwatch64_FILE, 1, vwatch64_HEADER"
    PRINT #BMFile, "            vwatch64_PREVLOF = LOF(vwatch64_FILE)"
    PRINT #BMFile, ""
    PRINT #BMFile, "            'Wait for data to be sent by host:"
    PRINT #BMFile, "            vwatch64_WAITFORDATA"
    PRINT #BMFile, ""
    PRINT #BMFile, "            GET #vwatch64_FILE, , vwatch64_RESPONSE"
    PRINT #BMFile, ""
    PRINT #BMFile, "            IF vwatch64_RESPONSE <> " + Q$ + "AUTHOK" + Q$ + " THEN"
    PRINT #BMFile, "                vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "                CLOSE #vwatch64_FILE"
    PRINT #BMFile, "            ELSE"
    PRINT #BMFile, "                vwatch64_AUTHORIZED = -1"
    PRINT #BMFile, "            END IF"
    PRINT #BMFile, "        ELSE"
    PRINT #BMFile, "            vwatch64_HEADER.CONNECTED = 0"
    PRINT #BMFile, "        END IF"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, ""
    PRINT #BMFile, "    SUB vwatch64_VARIABLEWATCH"
    PRINT #BMFile, "        PUT #vwatch64_FILE, vwatch64_CLIENTDATA, vwatch64_CLIENT"
    PRINT #BMFile, ""
    FOR i = 1 TO TotalVariables
        IF INSTR(vwatch64_VARIABLES(i).DATATYPE, "STRING") THEN
            SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").VALUE = " + RTRIM$(vwatch64_VARIABLES(i).NAME)
            PRINT #BMFile, SourceLine
        ELSE
            SourceLine = "    vwatch64_VARIABLES(" + LTRIM$(STR$(i)) + ").VALUE = STR$(" + RTRIM$(vwatch64_VARIABLES(i).NAME) + ")"
            PRINT #BMFile, SourceLine
        END IF
    NEXT i
    PRINT #BMFile, ""
    PRINT #BMFile, "        PUT #vwatch64_FILE, vwatch64_DATABLOCK, vwatch64_VARIABLES()"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, ""
    PRINT #BMFile, "    FUNCTION vwatch64_READKEYBOARD"
    PRINT #BMFile, "        vwatch64_READKEYBOARD = _KEYHIT"
    PRINT #BMFile, "    END FUNCTION"
    PRINT #BMFile, ""
    PRINT #BMFile, "    SUB vwatch64_WAITFORDATA"
    PRINT #BMFile, "        vwatch64_WAITSTART#=timer"
    PRINT #BMFile, "        DO: _LIMIT 30"
    PRINT #BMFile, "            vwatch64_LOF = LOF(vwatch64_FILE)"
    PRINT #BMFile, "            IF vwatch64_LOF > vwatch64_PREVLOF THEN"
    PRINT #BMFile, "                'Data received!"
    PRINT #BMFile, "                vwatch64_PREVLOF = vwatch64_LOF"
    PRINT #BMFile, "                EXIT DO"
    PRINT #BMFile, "            END IF"
    PRINT #BMFile, "            IF vwatch64_READKEYBOARD = 27 THEN EXIT DO"
    PRINT #BMFile, "        IF TIMER - vwatch64_WAITSTART# > 3 THEN EXIT DO"
    PRINT #BMFile, "        LOOP"
    PRINT #BMFile, "    END SUB"
    PRINT #BMFile, "$END IF"
    CLOSE BMFile
    PRINT "Done."
    SLEEP 1

    IF _FILEEXISTS("qb64.exe") THEN
        PRINT "Compiling...";
        IF SHELL("qb64.exe -c " + NEWFILENAME$) <> 0 THEN
            PRINT "failed."
            SLEEP 1
            SYSTEM
        ELSE
            IF _FILEEXISTS(LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4) + ".EXE") THEN
                SHELL _DONTWAIT LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4)
            ELSE
                PRINT "Could not find "; LEFT$(NEWFILENAME$, LEN(NEWFILENAME$) - 4) + ".EXE."
                SYSTEM
            END IF
        END IF
    END IF
    EXIT SUB
END SUB

FUNCTION vwatch64_CHECKKEYWORDS (Text$, List$())
    FOR i = 1 TO UBOUND(List$)
        IF INSTR(List$(i), Text$) THEN
            vwatch64_CHECKKEYWORDS = i
        END IF
    NEXT i
END FUNCTION
