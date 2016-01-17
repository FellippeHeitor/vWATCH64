'******************************************************************************
'*                                                                            *
'* Graphics Line Input Routines by Terry Ritchie, V2.0 06/11/12               *
'*                                                                            *
'* Revision history:                                                          *
'*                                                                            *
'* V1.0  - 05/06/12                                                           *
'*         Initial release                                                    *
'*                                                                            *
'* V2.0  - 06/11/12                                                           *
'*         Complete rewrite of code                                           *
'*         Added support for animated background images                       *
'*         Removed ON TIMER requirement leaving updating to programmer        *
'*         Added ability to create input fields that save/ignore background   *
'*         Added demo program to show features                                *
'*                                                                            *
'* V2.01 - 06/12/12                                                           *
'*         Fixed background color bug (background was always black)           *
'*         Streamlined code for better efficiency (faster)                    *
'*         Renamed input allowed constants (replaced _ with I)                *
'*         Subs and functions will exit immediatly if no active inputs        *
'*                                                                            *
'* V2.10 - 06/13/12                                                           *
'*         Added GLICURRENT command                                           *
'*         Created library documentation                                      *
'*                                                                            *
'* Email author at terry.ritchie@gmail.com with questions or concerns (bugs). *
'*                                                                            *
'* Written using QB64 V0.954 and released to public domain. No credit is      *
'* needed or expected for its use or modification.                            *
'*                                                                            *
'* Based on work started by SMcNeill at this thread:                          *
'* http://www.qb64.net/forum/index.php?topic=6018.0                           *
'*                                                                            *
'* Simulates the LINE INPUT command on a graphics screen at any x,y location. *
'* The routines identifies the current font and size in use when called and   *
'* adjust the input fields accordingly. Furthermore, operation is not         *
'* suspended at an input field, allowing the programmer to continue to        *
'* monitor and update background tasks.                                       *
'*                                                                            *
'* Keystrokes Supported:                                                      *
'*                                                                            *
'* INSERT      - alternates between INSERT/OVERWITE modes (cursor changes)    *
'* DELETE      - deletes the character at current cursor position             *
'* HOME        - moves cursor to beginning of input text line                 *
'* END         - moves cursor to end of input text line                       *
'* UP ARROW    - moves cursor to previous text input field (if multiple)      *
'* DOWN ARROW  - moves cursor to next text input field (if multiple)          *
'* RIGHT ARROW - moves cursor to the right one character                      *
'* LEFT ARROW  - moves cursor to the left one character                       *
'* BACKSPACE   - moves cursor to the left one character and deletes character *
'* TAB         - moves cursor to the next input field (if multiple)           *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'*                                COMMANDS:                                   *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLIINPUT - sets up a new input field location on the graphics screen.      *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* handle% = GLIINPUT(x%, y%, allowedtext%, displaytext$, save%)              *
'*                                                                            *
'* handle%      - number identifying this input field                         *
'* x%           - x coordinate location of graphic input field                *
'* y%           - y coordinate location of graphic input field                *
'* allowedtext% - number signifying what type of text input is allowed        *
'* displaytext$ - text to display at beginning of input field                 *
'* save%        - -1 (TRUE) to save background, 0 (FALSE) to disregard it     *
'*                                                                            *
'* The following values can be used with allowedtext%:                        *
'*                                                                            *
'* 1   - alphabetic characters only               (GLIALPHA)                  *
'* 2   - numeric characters only                  (GLINUMERIC)                *
'* 4   - symbolic characters only                 (GLISYMBOLS)                *
'* 8   - dash (-) symbol only                     (GLIDASH)                   *
'* 16  - parenthesis () only                      (GLIPAREN)                  *
'* 32  - force all input to lower case            (GLILOWER)                  *
'* 64  - force all input to upper case            (GLIUPPER)                  *
'* 128 - password field displaying asterisks only (GLIPASSWORD)               *
'*                                                                            *
'* You can combine any of these combinations together to make custom inputs:  *
'*                                                                            *
'* all chars - 7   (1+2+4     or GLIALPHA+GLINUMERIC+GLISYMBOLS)              *
'* phone #s  - 26  (2+8+16    or GLINUMERIC+GLIDASH+GLIPAREN)                 *
'* uppercase - 71  (1+2+4+64  or GLIALPHA+GLINUMERIC+GLISYMBOLS+GLIUPPER)     *
'* password  - 135 (1+2+4+128 or GLIALPHA+GLINUMERIC+GLISYMBOLS+GLIPASSWORD)  *
'*                                                                            *
'* Examples:                                                                  *
'*                                                                            *
'* MyName% = GLIINPUT(10, 100, 1, "Name:", -1)                                *
'* Password% = GLIINPUT(50, 250, 135, "Password:", 0)                         *
'* PhoneNum% = GLIINPUT(20, 200, 26, "Home Phone:", -1)                       *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLIOUTPUT$ - retrieves the text string from an input field.                *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* text$ = GLIOUTPUT$(handle%)                                                *
'*                                                                            *
'* text$   - the text returned by the routine                                 *
'* handle% - the number identifying the input field to get input text from    *
'*                                                                            *
'* Example:                                                                   *
'*                                                                            *
'* MyName$ = GLIOUTPUT$(MyName%)                                              *
'*                                                                            *
'* MyName% would have been previously created using GLIINPUT                  *
'*                                                                            *
'* Note:                                                                      *
'*                                                                            *
'* You can grab the output text from an input field at any time, but once the *
'* the field is closed the text is gone. (see GLICLOSE)                       *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLIENTERED - reports if the ENTER key has been pressed on a certain input  *
'*             field or on all active input fields. The result is a boolean   *
'*             true or false.                                                 *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* entered% = GLIENTERED(handle%)                                             *
'*                                                                            *
'* entered% - the result of the ENTER key being pressed (0=FALSE, -1=TRUE)    *
'* handle%  - the number identifying the input field to test for ENTER key    *
'*                                                                            *
'* The following values can be used for handle%:                              *
'*                                                                            *
'*  0 - test all active input fields for the ENTER key having been pressed    *
'* >0 - test an individual handle for the ENTER key having been pressed       *
'*                                                                            *
'* Example:                                                                   *
'*                                                                            *
'* IF GLIENTERED(MyName%) then MyName$ = GLOUPUT$(MyName%)                    *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLICLOSE - makes an input field stop allowing text input.                  *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* GLICLOSE handle%, behavior%                                                *
'*                                                                            *
'* handle%   - the number identifying the input field to close                *
'* behavior% - if set to TRUE (-1) the input field will disappear from screen *
'*                                                                            *
'* The following values can be used for handle%:                              *
'*                                                                            *
'*  0 - close all active input fields at once                                 *
'* >0 - close an individual input field                                       *
'*                                                                            *
'* Example:                                                                   *
'*                                                                            *
'* IF GLIENTERED(0) THEN '                  all fields been entered?          *
'*     MyName$ = GLOUPUT$(MyName%) '       yes, get the user name             *
'*     PhoneNum$ = GLIOUTPUT$(PhoneNum%) ' get the user phone number          *
'*     GLICLOSE 0, -1 '                    close and hide all input fields    *
'* END IF                                                                     *
'*                                                                            *
'* Note:                                                                      *
'*                                                                            *
'* Once all input fields have been closed the input text is lost forever.     *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLIFORCE - forces the cursor to move to the next input field or a specific *
'*            input field.                                                    *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* GLIFORCE handle%                                                           *
'*                                                                            *
'* handle% - the input field to force the cursor to move to                   *
'*                                                                            *
'* The following values can be used for handle%:                              *
'*                                                                            *
'* -1 - force the cursor to the next input field                              *
'* >0 - force the cursor to a specific input field                            *
'*                                                                            *
'* Example:                                                                   *
'*                                                                            *
'* IF LEN(GLOUPUT$(ssn%)) = 9 THEN GLIFORCE -1                                *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* GLICURRENT - returns the handle number of the current active input field.  *
'*              will return 0 if there are no active input fields.            *
'*                                                                            *
'* Usage:                                                                     *
'*                                                                            *
'* current% = GLICURRENT                                                      *
'*                                                                            *
'* Example:                                                                   *
'*                                                                            *
'* LOCATE 1, 1                                                                *
'* SELECT CASE GLICURRENT                                                     *
'*     CASE FirstName%                                                        *
'*         PRINT "Enter your first name"                                      *
'*     CASE LastName%                                                         *
'*         PRINT "Enter your last name"                                       *
'*     CASE Phone%                                                            *
'*         PRINT "Enter your phone number"                                    *
'* END SELECT                                                                 *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* Known Issues / TODOs:                                                      *
'*                                                                            *
'* None currently - Please report any bugs found. Also, if you make           *
'*                  improvements or modifications please send updates to      *
'*                  author. If used in an official update credit of course    *
'*                  will be given where credit is due.                        *
'*                                                                            *
'* -------------------------------------------------------------------------- *
'*                                                                            *
'* Correct usage of commands:                                                 *
'*                                                                            *
'* A proper order of commands must be maintained in order for the GLI library *
'* to work properly. GLICLEAR must be the first command in your loop and      *
'* GLIUPDATE must be the last command. All GLIINPUT fields must be set up     *
'* beforehand, outside of the loop. For example:                              *
'*                                                                            *
'* MyName% = GLIINPUT(25, 25, GLIALPHA, "First Name: ", -1)                   *
'*                                                                            *
'* DO                                                                         *
'*     GLICLEAR '        clears the input text and restores background        *
'*     .                                                                      *
'*     .                                                                      *
'*     <your code here>      Maintain this order in your loops                *
'*     .                                                                      *
'*     .                                                                      *
'*     GLIUPDATE '      displays the input text on screen                     *
'*     _DISPLAY '       updates the screen with GLIUPDATE results             *
'*                                                                            *
'* LOOP UNTIL GLIENTERED(MyName%)                                             *
'*                                                                            *
'* MyName$ = GLIOUTPUT$(MyName%)                                              *
'* GLICLOSE MyName%, -1                                                       *
'*                                                                            *
'******************************************************************************

CONST GLIALPHA = 1 '             alphabetic input allowed
CONST GLINUMERIC = 2 '           numeric input allowed
CONST GLISYMBOLS = 4 '           all symbols allowed
CONST GLIDASH = 8 '              dash (-) symbol allowed
CONST GLIPAREN = 16 '            parenthesis allowed
CONST GLILOWER = 32 '            lower case only
CONST GLIUPPER = 64 '            upper case only
CONST GLIPASSWORD = 128 '        password * only

TYPE GLI '                       graphics line input array type
    x AS INTEGER '               x location of text
    y AS INTEGER '               y location of text
    Allow AS INTEGER '           allowed characters in input field
    Text AS STRING * 255 '       display text
    InUse AS INTEGER '           is this field in use (T/F)
    CurrentFont AS LONG '        current font for this field
    FontHeight AS INTEGER '      current height of font for this field
    FontWidth AS INTEGER '       current width of font for this field
    MonoSpace AS INTEGER '       is the current font monospace (T/F)
    BackgroundColor AS LONG '    background color for this input field
    DefaultColor AS LONG '       foreground color for this input field
    TextWidth AS INTEGER '       width of display text
    CursorPosition AS INTEGER '  current cursor position within input field
    InputTextX AS INTEGER '      x location of input text field
    InputText AS STRING * 255 '  text being entered into input field
    ICursorHeight AS INTEGER '   INSERT mode cursor height
    OCursorHeight AS INTEGER '   OVERWRITE mode cursor height
    CursorX AS INTEGER '         cursor x location within input field
    ICursorY AS INTEGER '        INSERT mode cursor y location within input field
    OCursorY AS INTEGER '        OVERWRITE mode cursor y location within input field
    BlinkTimer AS SINGLE '       timer controllling cursor blink rate
    CursorWidth AS INTEGER '     width of cursor on current location within input field
    InsertMode AS INTEGER '      current cursor insert mode (0 = INSERT, -1 = OVERWRITE)
    Entered AS INTEGER '         ENTER has been pressed on this input field (T/F)
    TextImage AS LONG '          the image the text is drawn on
    Background AS LONG '         the original background image under text
    Save AS INTEGER '            TRUE to save background, False to overwrite background
    Visible AS INTEGER '         TRUE if input field visible on screen, FALSE otherwise
END TYPE

REDIM Gli(0) AS GLI '            text input array
DIM GliForced% '                 used to pass GLIFORCE messages to GLIUPDATE
DIM GliCur% '                    current field user typing into

