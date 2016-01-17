'------------------------------------------------------------------------------

FUNCTION GLICURRENT ()

'******************************************************************************
'*                                                                            *
'* Returns the handle number of the current active input field. The function  *
'* will return 0 if there are no active input fields.                         *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI
SHARED GliCur%

GLICURRENT = 0 '                                                               assume no active input fields
IF UBOUND(Gli) = 0 THEN EXIT FUNCTION '                                        leave if no active input fields
GLICURRENT = GliCur% '                                                         return handle of current active input field

END FUNCTION

'------------------------------------------------------------------------------

SUB GLICLEAR ()

'******************************************************************************
'*                                                                            *
'* Restores all input background images                                       *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI

DIM Scan% '                                                                    used to scan through input array

IF UBOUND(Gli) = 0 THEN EXIT SUB '                                             leave if nothing is active
FOR Scan% = 1 TO UBOUND(Gli) '                                                 cycle through input array
    _PUTIMAGE (Gli(Scan%).x, Gli(Scan%).y), Gli(Scan%).Background '            restore the destination (screen) background
NEXT Scan%

END SUB

'------------------------------------------------------------------------------

SUB GLIFORCE (handle%)

'******************************************************************************
'*                                                                            *
'* Forces cursor to move to a specific input field, the next input field or   *
'* forces the input array to reset.                                           *
'*                                                                            *
'* handle% values available to programmer:                                    *
'*                                                                            *
'* -1 = force to next input field                                             *
'* >0 = force to a specific input field                                       *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI
SHARED GliForced%

IF UBOUND(Gli) = 0 THEN EXIT SUB '                                             leave if nothing is active
IF (handle% < -1) OR (handle% = 0) OR (handle% > UBOUND(Gli)) THEN '           is handle% valid?
    BEEP '                                                                     no, get programmer's attention
    PRINT "ERROR: GLIFORCE - handle out of range" '                            display error on screen
    _DISPLAY
    END '                                                                      stop execution
END IF
GliForced% = handle% '                                                         inform GLIUPDATE of force behavior

END SUB

'------------------------------------------------------------------------------

SUB GLICLOSE (handle%, behavior%)

'******************************************************************************
'*                                                                            *
'* Closes all or a specific input field.                                      *
'*                                                                            *
'* handle% values available to programmer:                                    *
'*                                                                            *
'*  0 = close all input fields (forces a reset of input field array)          *
'* >0 = close a specific input field                                          *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI
SHARED GliCur%

DIM Scan% '                                                                    used to scan through input array

IF UBOUND(Gli) = 0 THEN EXIT SUB '                                             leave if nothing is active
IF handle% <> 0 THEN '                                                         closing all input fields?
    IF (handle% < 0) OR (handle% > UBOUND(Gli)) THEN '                         no, is handle% valid?
        BEEP '                                                                 no, get programmer's attention
        PRINT "ERROR: GLICLOSE - handle out of range" '                        display error on screen
        _DISPLAY '                                                             make sure programmer sees it
        END '                                                                  stop execution
    ELSEIF NOT Gli(handle%).InUse THEN '                                       yes, is input field in use?
        BEEP '                                                                 no, get programmer's attention
        PRINT "ERROR: GLICLOSE - handle already closed" '                      display error on screen
        _DISPLAY '                                                             make sure programmer sees it
        END '                                                                  stop execution
    END IF
END IF
IF handle% > 0 THEN '                                                          closing a specific input field?
    Gli(handle%).InUse = 0 '                                                   yes, this input field no longer used (FALSE)
    IF behavior% THEN '                                                        should text be hidden?
        _PUTIMAGE (Gli(handle%).x, Gli(handle%).y), Gli(handle%).Background '  yes, restore original background image
        Gli(handle%).Visible = 0 '                                             set this input field to invisible (FALSE)
    END IF
    FOR Scan% = 1 TO UBOUND(Gli) '                                             cycle through the input array
        IF Gli(Scan%).InUse THEN '                                             is this input field in use?
            GLIFORCE -1 '                                                      yes, force input to next field
            EXIT SUB '                                                         no need to scan any further
        END IF
    NEXT Scan%
END IF
FOR Scan% = 1 TO UBOUND(Gli) '                                                 cycle through all input fields
    IF behavior% AND Gli(Scan%).Visible THEN '                                 make a visible input invisible?
        _PUTIMAGE (Gli(Scan%).x, Gli(Scan%).y), Gli(Scan%).Background '        yes, restore original background
    END IF
    _FREEIMAGE Gli(Scan%).TextImage '                                          remove the text image from memory
    _FREEIMAGE Gli(Scan%).Background '                                         remove the background image from memory
NEXT Scan%
REDIM Gli(0) AS GLI '                                                          reset the input array
GliCur% = 0 '                                                                  reset the current input field

END SUB

'------------------------------------------------------------------------------

FUNCTION GLIOUTPUT$ (handle%)

'******************************************************************************
'*                                                                            *
'* Retrieves the input text from a specific input field.                      *
'*                                                                            *
'* handle% values available to programmer:                                    *
'*                                                                            *
'* >0 = get the input text from the specific input field                      *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI

DIM InputText$ '                                                               holds cleaned input text from array

IF UBOUND(Gli) = 0 THEN EXIT FUNCTION '                                        leave if nothing is active
IF (handle% < 1) OR (handle% > UBOUND(Gli)) THEN '                             is handle% valid?
    BEEP '                                                                     no, get programmer's attention
    PRINT "ERROR: GLIOUTPUT$ - handle out of range" '                          display error on screen
    _DISPLAY
    END '                                                                      stop execution
ELSEIF NOT Gli(handle%).InUse THEN '                                           yes, is input field in use?
    BEEP '                                                                     no, get programmer's attention
    PRINT "ERROR: GLIOUTPUT$ - handle not in use" '                            display error on screen
    _DISPLAY
    END '                                                                      stop execution
END IF
InputText$ = RTRIM$(Gli(handle%).InputText) '                                  trim excess spaces from end of text input
GLIOUTPUT$ = LEFT$(InputText$, LEN(InputText$) - 1) '                          remove chr(0) place holder and return value

END FUNCTION

'------------------------------------------------------------------------------

FUNCTION GLIENTERED (handle%)

'******************************************************************************
'*                                                                            *
'* Reports back if the ENTER key has been pressed on one or all input fileds. *
'*                                                                            *
'* handle% values available to programmer:                                    *
'*                                                                            *
'*  0 = get the ENTER key status of all input fields in use (TRUE/FALSE)      *
'* >0 = get the ENTER key status on a certain input field (TRUE/FALSE)        *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI

DIM Scan% '                                                                    used to scan through input array

IF UBOUND(Gli) = 0 THEN EXIT FUNCTION '                                        leave if nothing is active
IF (handle% < 0) OR (handle% > UBOUND(Gli)) THEN '                             is handle% valid?
    BEEP '                                                                     no, get programmer's attention
    PRINT "ERROR: GLIENTERED - handle out of range" '                          display error on screen
    _DISPLAY
    END '                                                                      stop execution
END IF
IF handle% > 0 THEN '                                                          looking for a certain input field?
    GLIENTERED = Gli(handle%).Entered '                                        yes, report back the ENTER key status
ELSE '                                                                         no, looking for all input fields
    GLIENTERED = -1 '                                                          assume all have had ENTER key pressed (TRUE)
    FOR Scan% = 1 TO UBOUND(Gli) '                                             scan the entire input array
        IF Gli(Scan%).InUse AND (NOT Gli(Scan%).Entered) THEN '                is field in use and no ENTER key pressed?
            GLIENTERED = 0 '                                                   yes, report back not all fields been ENTERed (FALSE)
            EXIT FUNCTION '                                                    no need to check any further
        END IF
    NEXT Scan%
END IF

END FUNCTION

'------------------------------------------------------------------------------

FUNCTION GLIINPUT (x%, y%, allow%, text$, Save%) STATIC

'******************************************************************************
'*                                                                            *
'* Sets up a text input at the coordinates given, allowed input text and      *
'* display text. Returns a handle value that points to the input text field.  *
'*                                                                            *
'* x%     - x location of input text field                                    *
'* y%     - y location of input text field                                    *
'* allow% - type of text allowed                                              *
'* text$  - string of text to display in front of input field                 *
'* Save%  - TRUE to save background image, FALSE to overwrite background      *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI
SHARED GliCur%

DIM c% '                                                                       the new handle number

IF GliCur% = 0 THEN GliCur% = 1 '                                              first time called set to 1
REDIM _PRESERVE Gli(UBOUND(Gli) + 1) AS GLI '                                  create a new input array entry
c% = UBOUND(Gli) '                                                             get the new handle number
Gli(c%).x = x% '                                                               save the x location of text
Gli(c%).y = y% '                                                               save the y location of text
Gli(c%).Allow = allow% '                                                       save the type of input allowed
Gli(c%).Text = text$ + CHR$(0) '                                               pad the text with a chr(0)
Gli(c%).InUse = -1 '                                                           this input field is now in use (TRUE)
Gli(c%).CurrentFont = _FONT(_DEST) '                                           get the font currently in use
Gli(c%).FontHeight = _FONTHEIGHT(_FONT(_DEST)) '                               get the height of the current font
Gli(c%).FontWidth = _FONTWIDTH(_FONT(_DEST)) '                                 get the width of the current font
IF Gli(c%).FontWidth > 0 THEN Gli(c%).MonoSpace = -1 '                         identify monospace fonts (TRUE)
Gli(c%).BackgroundColor = _BACKGROUNDCOLOR '                                   get the current background color
Gli(c%).DefaultColor = _DEFAULTCOLOR '                                         get the current foreground color
Gli(c%).TextWidth = _PRINTWIDTH(text$) '                                       get the width of the text
Gli(c%).CursorPosition = 1 '                                                   set the cursor at the beginning of the input line
Gli(c%).InputTextX = _PRINTWIDTH(text$) '                                      get the x location of the input text line
Gli(c%).InputText = CHR$(0) '                                                  set input text as empty
Gli(c%).ICursorHeight = INT(_FONTHEIGHT(_FONT(_DEST)) / 12) - 1 '              compute the insert cursor height
IF Gli(c%).ICursorHeight < 0 THEN Gli(c%).ICursorHeight = 0 '                  correct cursor height for very small fonts
Gli(c%).OCursorHeight = _FONTHEIGHT(_FONT(_DEST)) - 1 '                        compute the overwrite cursor height
Gli(c%).CursorX = _PRINTWIDTH(text$) '                                         compute the x location of cursor
Gli(c%).ICursorY = _FONTHEIGHT(_FONT(_DEST)) - Gli(c%).ICursorHeight - 1 '     compute the y location of insert cursor
Gli(c%).OCursorY = 0 '                                                         save the y location of the overwrite cursor
Gli(c%).BlinkTimer = TIMER '                                                   save the cursor blink timer
Gli(c%).CursorWidth = _PRINTWIDTH("1") - 1 '                                   save the initial width of the cursor
Gli(c%).InsertMode = 0 '                                                       initial insert mode to insert
Gli(c%).Entered = 0 '                                                          ENTER has not been pressed yet (FALSE)
Gli(c%).Save = Save% '                                                         get the background saving behavior
Gli(c%).Visible = -1 '                                                         initially visible on screen (TRUE)
Gli(c%).TextImage = _NEWIMAGE(1, 1, 32) '                                      create initial text image holder
Gli(c%).Background = _NEWIMAGE(1, 1, 32) '                                     create initial background image holder
GLIINPUT = c% '                                                                return with the new handle number

END FUNCTION

'------------------------------------------------------------------------------

SUB GLIUPDATE ()

'******************************************************************************
'*                                                                            *
'* Updates the inputs on screen                                               *
'*                                                                            *
'******************************************************************************

SHARED Gli() AS GLI
SHARED GliForced%
SHARED GliCur%

DIM Scan% '                                                                    used to scan input array
DIM CursorWidth% '                                                             holds width of cursor at current text position
DIM Text$ '                                                                    holds clean text from text within array
DIM InputText$ '                                                               holds clean input text from input text within array
DIM OriginalFont& '                                                            the font before this subroutine called
DIM Spc$ '                                                                     space padding needed at end of line
DIM OriginalDest& '                                                            the image destination before subroutine called

IF UBOUND(Gli) = 0 THEN EXIT SUB '                                             leave if nothing is active
IF GliCur% = 0 THEN GliCur% = 1 '                                              if this is first time set current input to 1
OriginalDest& = _DEST '                                                        save current desitnation image
OriginalFont& = _FONT(_DEST) '                                                 save current font
FOR Scan% = 1 TO UBOUND(Gli) '                                                 cycle through all inputs
    IF Gli(Scan%).Visible THEN '                                               is this input visible?
        _FREEIMAGE Gli(Scan%).TextImage '                                      yes, free input's text image from memory
        _FREEIMAGE Gli(Scan%).Background '                                     free input's background image from memory
        InputText$ = RTRIM$(Gli(Scan%).InputText) '                            get clean input text from array
        InputText$ = LEFT$(InputText$, LEN(InputText$) - 1) '                  remove the chr(0) placeholder
        Text$ = RTRIM$(Gli(Scan%).Text) '                                      get clean text from array
        Text$ = LEFT$(Text$, LEN(Text$) - 1) '                                 remove the chr(0) placeholder
        _FONT Gli(Scan%).CurrentFont '                                         set font for this input field
        IF Gli(Scan%).MonoSpace THEN Spc$ = "  " ELSE Spc$ = SPACE$(10) '      set up input trailing spaces
        Gli(Scan%).TextImage = _NEWIMAGE(_PRINTWIDTH(Text$ + InputText$ +_
            "W"), Gli(Scan%).FontHeight, 32) '                                 create new text image
        _DEST Gli(Scan%).TextImage '                                           set the text image as the destination
        _FONT Gli(Scan%).CurrentFont, Gli(Scan%).TextImage '                   apply font to the new text image
        _PUTIMAGE (0, 0), OriginalDest&, Gli(Scan%).TextImage, (Gli(Scan%).x,_
            Gli(Scan%).y)-(Gli(Scan%).x + _PRINTWIDTH(Text$ + InputText$ +_
            "W") - 1, Gli(Scan%).y + Gli(Scan%).FontHeight - 1) '              get the background image
        Gli(Scan%).Background = _COPYIMAGE(Gli(Scan%).TextImage) '             copy a clean background image
        IF Gli(Scan%).Save THEN '                                              should input save the background?
            _PRINTMODE _KEEPBACKGROUND , Gli(Scan%).TextImage '                set font to save the background
        ELSE '                                                                 no, background is to be ignored
            LINE (0, 0)-(_WIDTH(_DEST), _HEIGHT(_DEST)),_
                Gli(c%).BackgroundColor, BF '                                  set text background color
        END IF
        COLOR Gli(Scan%).DefaultColor, Gli(Scan%).BackgroundColor '            set the foreground and background colors
        _PRINTSTRING (0, 0), Text$, Gli(Scan%).TextImage '                     display the leading text (if any)
        IF Gli(Scan%).Allow AND 128 THEN '                                     is this a password field?
            _PRINTSTRING (Gli(Scan%).InputTextX, 0), STRING$(LEN(InputText$),_
                "*") + Spc$, Gli(Scan%).TextImage '                            yes, display asterisks only
            CursorWidth% = _PRINTWIDTH("*") - 1 '                              cursor always width of an asterisk
            Gli(Scan%).CursorX = Gli(Scan%).InputTextX +_
                _PRINTWIDTH(LEFT$(STRING$(LEN(InputText$), "*"),_
                Gli(Scan%).CursorPosition - 1)) '                              compute cursor x location
        ELSE '                                                                 no, this is not a password field
            _PRINTSTRING (Gli(Scan%).InputTextX, 0), InputText$ +_
                Spc$, Gli(Scan%).TextImage '                                   display actual input text
            CursorWidth% = _PRINTWIDTH(MID$(InputText$,_
                Gli(Scan%).CursorPosition, 1)) - 1 '                           cursor width based on size of current position
            IF CursorWidth% <= 0 THEN CursorWidth% = _PRINTWIDTH("1") - 1 '    at end of line? if so set cursor width to default size
            Gli(Scan%).CursorX = Gli(Scan%).InputTextX +_
                _PRINTWIDTH(LEFT$(InputText$, Gli(Scan%).CursorPosition - 1)) 'compute cursor x location
        END IF
        _PUTIMAGE (Gli(Scan%).x, Gli(Scan%).y),_
            Gli(Scan%).TextImage, OriginalDest& '                              place the text image on the destination image (screen)
    END IF
NEXT Scan%
InputText$ = RTRIM$(Gli(GliCur%).InputText) '                                  get clean input text from array
InputText$ = LEFT$(InputText$, LEN(InputText$) - 1) '                          remove the chr(0) placeholder
_FONT Gli(GliCur%).CurrentFont '                                               set current input field font
IF Gli(GliCur%).Allow AND 128 THEN '                                           is this a password field?
    CursorWidth% = _PRINTWIDTH("*") - 1 '                                      yes, cursor always width of an asterisk
ELSE '                                                                         no, this is not a password field
    CursorWidth% = _PRINTWIDTH(MID$(InputText$,_
       Gli(GliCur%).CursorPosition, 1)) - 1 '                                  cursor width based on size of current position
    IF CursorWidth% <= 0 THEN CursorWidth% = _PRINTWIDTH("1") - 1 '            at end of line? if so set cursor width to default size
END IF
_DEST Gli(GliCur%).TextImage '                                                 set the text image as new destination
IF Gli(GliCur%).InsertMode = 0 THEN '                                          in INSERT mode?
    cy% = Gli(GliCur%).ICursorY '                                              yes, use insert mode cursor y position
    ch% = Gli(GliCur%).ICursorHeight '                                         use insert mode cursor height
ELSE '                                                                         no, in OVERWRITE mode
    cy% = Gli(GliCur%).OCursorY '                                              use overwrite mode cursor y position
    ch% = Gli(GliCur%).OCursorHeight '                                         use overwrite mode cursor height
END IF
SELECT CASE TIMER '                                                            look at the value in timer
    CASE IS < Gli(GliCur%).BlinkTimer + .15 '                                  has 0 to .15 second elapsed?
        IF Gli(GliCur%).CursorPosition > LEN(InputText$) THEN '                yes, is the cursor at the end of line?
            LINE (Gli(GliCur%).CursorX, cy%)-(Gli(GliCur%).CursorX +_
                CursorWidth%, cy% + ch%),_
                Gli(GliCur%).BackgroundColor, BF '                             yes, erase the cursor
        END IF
    CASE IS < Gli(GliCur%).BlinkTimer + .3 '                                   has .16 to .3 second elapsed?
        LINE (Gli(GliCur%).CursorX, cy%)-(Gli(GliCur%).CursorX +_
            CursorWidth%, cy% + ch%), Gli(GliCur%).DefaultColor, BF '          yes, draw the cursor
    CASE ELSE '                                                                greater than .3 second has elapsed
        Gli(GliCur%).BlinkTimer = TIMER '                                      reset the array blink timer
END SELECT
_DEST OriginalDest& '                                                          restore original destination
_FONT OriginalFont& '                                                          restore calling procedure font
_PUTIMAGE (Gli(GliCur%).x, Gli(GliCur%).y), Gli(GliCur%).TextImage '           place the text image on the destination image (screen)
IF GliForced% <> 0 THEN '                                                      being forced to an input field?
    IF GliForced% = -1 THEN '                                                  yes, to the next one?
        Scan% = GliCur% '                                                      set scanner to current input field
        DO '                                                                   start scanning
            Scan% = Scan% + 1 '                                                move scanner to next handle number
            IF Scan% > UBOUND(Gli) THEN Scan% = 1 '                            return to start of input array if limit reached
            IF Gli(Scan%).InUse THEN GliCur% = Scan% '                         set current input field if in use
        LOOP UNTIL GliCur% = Scan% '                                           leave scanner when an input field in use is found
        GliForced% = 0 '                                                       reset force indicator
    ELSE '                                                                     yes, to a specific input field
        GliCur% = GliForced% '                                                 set the current input field
        GliForced% = 0 '                                                       reset force indicator
    END IF
END IF
KeyHit& = _KEYHIT '                                                            check for a key having been pressed
IF KeyHit& > 0 THEN '                                                          was a key pressed?
    Gli(GliCur%).InsertMode = _KEYDOWN(200012) '                               yes, get the insert mode
    SELECT CASE KeyHit& '                                                      which key was hit?
        CASE 20992 '                                                           INSERT key was pressed
            EXIT SUB '                                                         user changed insert mode, leave subroutine
        CASE 19712 '                                                           RIGHT ARROW key was pressed
            Gli(GliCur%).CursorPosition = Gli(GliCur%).CursorPosition + 1 '    increment the cursor position
            IF Gli(GliCur%).CursorPosition% > LEN(InputText$) + 1 THEN '       will this take the cursor too far?
                Gli(GliCur%).CursorPosition = LEN(InputText$) + 1 '            yes, keep the cursor at the end of the line
            END IF
        CASE 19200 '                                                           LEFT ARROW key was pressed
            Gli(GliCur%).CursorPosition = Gli(GliCur%).CursorPosition - 1 '    decrement the cursor position
            IF Gli(GliCur%).CursorPosition = 0 THEN '                          did cursor go beyone beginning of line?
                Gli(GliCur%).CursorPosition = 1 '                              yes, keep the cursor at the beginning of the line
            END IF
        CASE 8 '                                                               BACKSPACE key pressed
            IF Gli(GliCur%).CursorPosition > 1 THEN '                          is the cursor at the beginning of the line?
                InputText$ = LEFT$(InputText$,_
                    Gli(GliCur%).CursorPosition - 2) +_
                    RIGHT$(InputText$, LEN(InputText$) -_
                    Gli(GliCur%).CursorPosition% + 1) '                        no, delete character
                Gli(GliCur%).InputText = InputText$ + CHR$(0) '                save new input text into input array
                Gli(GliCur%).CursorPosition = Gli(GliCur%).CursorPosition - 1 'decrement the cursor position
            END IF
        CASE 18176 '                                                           HOME key was pressed
            Gli(GliCur%).CursorPosition = 1 '                                  move the cursor to the beginning of the line
        CASE 20224 '                                                           END key was pressed
            Gli(GliCur%).CursorPosition = LEN(InputText$) + 1 '                move the cursor to the end of the line
        CASE 21248 '                                                           DELETE key was pressed
            IF Gli(GliCur%).CursorPosition < LEN(InputText$) + 1 THEN '        is the cursor at the end of the line?
                InputText$ = LEFT$(InputText$,_
                Gli(GliCur%).CursorPosition - 1) +_
                RIGHT$(InputText$, LEN(InputText$) -_
                Gli(GliCur%).CursorPosition) '                                 no, delete character
                Gli(GliCur%).InputText = InputText$ + CHR$(0) '                save new input text into input array
            END IF
        CASE 9, 13, 20480 '                                                    TAB, ENTER or DOWN ARROW key pressed
            IF KeyHit& = 13 THEN Gli(GliCur%).Entered = -1 '                   if enter key was pressed remember it (TRUE)
            Scan% = GliCur% '                                                  set initital point of input array scan
            DO '                                                               begin scanning input array
                Scan% = Scan% + 1 '                                            increment the scanner
                IF Scan% > UBOUND(gli) THEN Scan% = 1 '                        go to beginning of array if the end was reached
                IF Gli(Scan%).InUse THEN GliCur% = Scan% '                     if this field is in use then set it as the current input field
            LOOP UNTIL GliCur% = Scan% '                                       keep scanning until a valid field is found
            Gli(GliCur%).InsertMode = _KEYDOWN(200012) '                       save the current insert mode to use in this field
        CASE 18432 '                                                           UP ARROW key was pressed
            Scan% = GliCur% '                                                  set initial point of input array scan
            DO '                                                               begin scanning input array
                Scan% = Scan% - 1 '                                            decrement the scanner
                IF Scan% = 0 THEN Scan% = UBOUND(gli) '                        go the end of the array if the beginning was reached
                IF Gli(Scan%).InUse THEN GliCur% = Scan% '                     if this field is in use then set it as the current input field
            LOOP UNTIL GliCur% = Scan% '                                       keep scanning until a valid field is found
            Gli(GliCur%).InsertMode = _KEYDOWN(200012) '                       save the current insert mode to use in this field
        CASE ELSE '                                                            a character key was pressed
            IF KeyHit& > 31 AND KeyHit& < 256 THEN '                           is it a valid ASCII displayable character?
                KeyHit$ = "" '                                                 yes, initialize key holder variable
                SELECT CASE KeyHit& '                                          which alphanumeric key was pressed?
                    CASE 32 '                                                  SPACE key was pressed
                        KeyHit$ = CHR$(KeyHit&) '                              save the keystroke
                    CASE 40 TO 41 '                                            PARENTHESIS key was pressed
                        IF (Gli(GliCur%).Allow AND 4) OR_
                            (Gli(GliCur%).Allow AND 16) THEN_
                            KeyHit$ = CHR$(KeyHit&) '                          if it's allowed then save the keystroke
                    CASE 45 '                                                  DASH (minus -) key was pressed
                        IF Gli(GliCur%).Allow AND 8 THEN '                     are dashes allowed?
                            KeyHit$ = CHR$(KeyHit&) '                          yes, save the keystroke
                        END IF
                    CASE 48 TO 57 '                                            NUMBER key was pressed
                        IF Gli(GliCur%).Allow AND 2 THEN '                     are numbers allowed?
                            KeyHit$ = CHR$(KeyHit&) '                          yes, save the keystroke
                        END IF
                    CASE 33 TO 47, 58 TO 64, 91 TO 96, 123 TO 255 '            SYMBOL key was pressed
                        IF Gli(GliCur%).Allow AND 4 THEN '                     are symbols allowed?
                            KeyHit$ = CHR$(KeyHit&) '                          yes, save the keystroke
                        END IF
                    CASE 65 TO 90, 97 TO 122 '                                 ALPHABETIC key was pressed
                        IF Gli(GliCur%).Allow AND 1 THEN '                     are alpha keys allowed?
                            KeyHit$ = CHR$(KeyHit&) '                          yes, save the keystroke
                        END IF
                END SELECT
                IF KeyHit$ <> "" THEN '                                        was an allowed keystroke saved?
                    IF Gli(GliCur%).Allow AND 32 THEN '                        should it be forced to lower case?
                        KeyHit$ = LCASE$(KeyHit$) '                            yes, force the keystroke to lower case
                    END IF
                    IF Gli(GliCur%).Allow AND 64 THEN '                        should it be forced to upper case?
                        KeyHit$ = UCASE$(KeyHit$) '                            yes, force the keystroke to upper case
                    END IF
                    IF Gli(GliCur%).CursorPosition = LEN(InputText$) + 1 THEN 'is the cursor at the end of the line?
                        InputText$ = InputText$ + KeyHit$ '                    yes, simply add the keystroke to input text
                        Gli(GliCur%).InputText = InputText$ + CHR$(0) '        pad input text with chr(0) and save new input text in array
                        Gli(GliCur%).CursorPosition = _
                            Gli(GliCur%).CursorPosition + 1 '                  increment the cursor position
                    ELSEIF Gli(GliCur%).InsertMode = 0 THEN '                  no, are we in INSERT mode?
                        InputText$ = LEFT$(InputText$,_
                            Gli(GliCur%).CursorPosition - 1) + KeyHit$ +_
                            RIGHT$(InputText$, LEN(InputText$) -_
                            Gli(GliCur%).CursorPosition + 1) '                 yes, insert the character
                        Gli(GliCur%).InputText = InputText$ + CHR$(0) '        pad input text with chr(0) and save new input text in array
                        Gli(GliCur%).CursorPosition =_
                            Gli(GliCur%).CursorPosition + 1 '                  increment the cursor position
                    ELSE '                                                     no, we are in OVERWRITE mode
                        InputText$ = LEFT$(InputText$,_
                            Gli(GliCur%).CursorPosition - 1) + KeyHit$ +_
                            RIGHT$(InputText$, LEN(InputText$) -_
                            Gli(GliCur%).CursorPosition) '                     overwrite with new character
                        Gli(GliCur%).InputText = InputText$ + CHR$(0) '        pad input text with chr(0) and save new input text in array
                        Gli(GliCur%).CursorPosition =_
                            Gli(GliCur%).CursorPosition + 1 '                  increment the cursor position
                    END IF
                END IF
            END IF
    END SELECT
END IF

END SUB

