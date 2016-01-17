'******************************************************************************
'*                                                                            *
'* QB64 Menu Routines by Terry Ritchie, V1.0 08/01/12                         *
'*                                                                            *
'* Revision history:                                                          *
'*                                                                            *
'* V1.0  - 08/01/12                                                           *
'*         Initial release                                                    *
'*                                                                            *
'* Email author at terry.ritchie@gmail.com with questions or concerns (bugs). *
'*                                                                            *
'* Written using QB64 V0.954 and released to public domain. No credit is      *
'* needed or expected for its use or modification.                            *
'*                                                                            *
'* Creates a Windows-like menu system to be used in QB64 programs.            *
'*                                                                            *
'******************************************************************************

FUNCTION GETMENUACTIVE% ()

'******************************************************************************
'*                                                                            *
'* Retreives whether menu is currently active (in use) (-1) TRUE, (0) FALSE   *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

GETMENUACTIVE% = Mset.menuactive

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION GETMENUSHOWING% ()

'******************************************************************************
'*                                                                            *
'* Retreives whether the menu is showing or not (-1) TRUE, (0) FALSE          *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

GETMENUSHOWING% = Mset.showing

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

SUB HIDEMENU ()

'******************************************************************************
'*                                                                            *
'* Removes the menu from the screen and restores the background image that    *
'* was under the top bar and any submenu showing.                             *
'*                                                                            *
'******************************************************************************

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

'Mset.menuactive = -1 - Mset.menuactive '                                       toggle active state of menu
IF Mset.submenuactive THEN '                                                   is a submenu showing?
    _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).undersubmenu ' yes, remove it
END IF
Mset.submenu = 0 '                                                             reset current submenu entry value
Mset.oldsubmenu = 0 '                                                          reset previous submenu entry value
Mset.menuactive = 0 '
Mset.submenuactive = 0 '                                                       disable active state of submenu
_PUTIMAGE (0, 0), Mset.undermenu '                                             restore background under top menu bar
Mset.showing = 0 '                                                             menu is no longer showing on screen

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SHOWMENU ()

'******************************************************************************
'*                                                                            *
'* Places the menu bar at the top of the current screen and activates the     *
'* menu system. The image under the menu bar will be saved and can be         *
'* restored with HideMenu().                                                  *
'*                                                                            *
'******************************************************************************

SHARED Mset.menubar
SHARED Mset AS MENUSETTINGS

_PUTIMAGE (0, 0), _DEST, Mset.undermenu, (0, 0)-(_WIDTH(Mset.menubar) - 1, _HEIGHT(Mset.menubar) - 1) ' save background under top menu bar
_PUTIMAGE (0, 0), Mset.menubar '                                               put the top menu bar on screen
Mset.showing = -1 '                                                            menu is now showing on screen

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION GETMENUHEIGHT% ()

'******************************************************************************
'*                                                                            *
'* Retreives the menu bar height.                                             *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

GETMENUHEIGHT% = Mset.height '                                                 return the height of the top menu bar

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION GETCURRENTMENU% ()

'******************************************************************************
'*                                                                            *
'* Retreives the menu number user is currently active on.                     *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

GETCURRENTMENU% = Mset.mainmenu * 100 + Mset.submenu '                         return the current menu user in on

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUSHADOW (height%)

'******************************************************************************
'*                                                                            *
'* Sets the height of the submenu above the background, creating a shadow     *
'* underneath. A height% of 0 effectively turns off shadowing.                *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

IF height% < 0 THEN height% = 0 '                                              height must be no less than 0
Mset.shadow = height% '                                                        set the submenu shadow height
IF (Mset.called AND 1024) = 0 THEN Mset.called = Mset.called + 1024 '          remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETSUBMENULOCATION (tweak%)

'******************************************************************************
'*                                                                            *
'* Tweaks the location of the submenu Y location by tweak% amount. Positive   *
'* values lower the submenus, negative values raise the submenus.             *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.subtweak = tweak% '                                                      set the Y tweak amount
IF (Mset.called AND 512) = 0 THEN Mset.called = Mset.called + 512 '           remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUSPACING (spacing%)

'******************************************************************************
'*                                                                            *
'* Sets the amount a space, in pixels, to the right and left of main and sub  *
'* menu entries.                                                              *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

IF spacing% < 0 THEN spacing% = 0 '                                            spacing must be no less than 0
Mset.spacing = spacing% '                                                      set the menu spacing amount
IF (Mset.called AND 32) = 0 THEN Mset.called = Mset.called + 32 '              remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETSUBMENUCOLORS (smatext~&, smhtext~&, smitext~&, smihtext~&, smabg~&, smhbg~&, smibg~&, smihbg~&)

'******************************************************************************
'*                                                                            *
'* Set the text and background colors for the sub menu.                       *
'*                                                                            *
'* The colors values are passed in this order:                                *
'*                                                                            *
'* smatext~&  = sub menu active (normal) text color                           *
'* smhtext~&  = sub menu highlight text color                                 *
'* smitext~&  = sub menu inactive text color                                  *
'* smihtext~& = sub menu inactive highlight text color                        *
'* smabg~&    = sub menu active background color                              *
'* smhbg~&    = sub menu highlight background color                           *
'* smibg~&    = sub menu inactive background color                            *
'* smihbg~&   = sub menu inactive highlight background color                  *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.smatext = smatext~& '                                                     set the active (normal) sub menu text color
Mset.smhtext = smhtext~& '                                                     set the highlight sub menu text color
Mset.smitext = smitext~& '                                                     set the inactive sub menu text color
Mset.smihtext = smihtext~& '                                                   set the inactive highlight sub menu text color
Mset.smabg = smabg~& '                                                         set the active (normal) sub menu background color
Mset.smhbg = smhbg~& '                                                         set the highlight sub menu background color
Mset.smibg = smibg~& '                                                         set the inactive sub menu background color
Mset.smihbg = smihbg~& '                                                       set the inactive highlight sub menu background color
IF (Mset.called AND 4) = 0 THEN Mset.called = Mset.called + 4 '                remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMAINMENUCOLORS (mmatext~&, mmhtext~&, mmstext~&, mmabarbg~&, mmhbarbg~&, mmsbarbg~&)

'******************************************************************************
'*                                                                            *
'* Set the text and background colors for the main menu top bar.              *
'*                                                                            *
'* The colors values are passed in this order:                                *
'*                                                                            *
'* mmatext~&  = main menu active (normal) text color                          *
'* mmhtext~&  = main menu highlight text color                                *
'* mmstext~&  = main menu selected text color                                 *
'* mmabarbg~& = main menu active (normal) background color                    *
'* mmhbarbg~& = main menu highlight background color                          *
'* mmsbarbg~& = main menu selected background color                           *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.mmatext = mmatext~& '                                                     set main menu active (normal) text color
Mset.mmhtext = mmhtext~& '                                                     set main menu highlight text color
Mset.mmstext = mmstext~& '                                                     set main menu selected text color
Mset.mmabarbg = mmabarbg~& '                                                   set main menu active menu bar background color
Mset.mmhbarbg = mmhbarbg~& '                                                   set main menu highlight menu bar backgrund color
Mset.mmsbarbg = mmsbarbg~& '                                                   set main menu selected menu bar background color
IF (Mset.called AND 2) = 0 THEN Mset.called = Mset.called + 2 '                remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENU3D (mmbar%, smbar%, mm%, sm%)

'******************************************************************************
'*                                                                            *
'* If TRUE (-1) is passed the menu will take on a 3D effect. Passing a value  *
'* of FALSE (0) will make the menu take on a flat look.                       *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.mmbar3D = mmbar% '                                                        main menu bar to 3D
Mset.smbar3D = smbar% '                                                        sub menu boxes to 3D
Mset.mm3D = mm% '                                                              main menu entries to 3D
Mset.sm3D = sm% '                                                              sub menu entries to 3D
IF (Mset.called AND 8) = 0 THEN Mset.called = Mset.called + 8 '                remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUTEXT (tweak%)

'******************************************************************************
'*                                                                            *
'* Tweaks the location of the menu and sub menu text by tweak% amount.        *
'* Positive values lower the text, negative values raise the text.            *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.texttweak = tweak% '                                                      set amount of Y text shifting
IF (Mset.called AND 128) = 0 THEN Mset.called = Mset.called + 128 '            remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUUNDERSCORE (tweak%)

'******************************************************************************
'*                                                                            *
'* Tweaks the location of the ALT key underscore by Tweak% amount. Positive   *
'* values lower the underscore, negative values raise the underscore.         *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

Mset.alttweak = tweak% '                                                       set amount of Y ALT underscore shifting
IF (Mset.called AND 256) = 0 THEN Mset.called = Mset.called + 256 '            remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUINDENT (Indent%)

'******************************************************************************
'*                                                                            *
'* Forces the height of each meny entry to Height%                            *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

IF Indent% < 0 THEN Indent% = 0 '                                              must be 0 or higher
Mset.indent = Indent% '                                                        set amount of menu indention
IF (Mset.called AND 64) = 0 THEN Mset.called = Mset.called + 64 '              remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUHEIGHT (Height%)

'******************************************************************************
'*                                                                            *
'* Forces the height of each menu entry to Height%                            *
'*                                                                            *
'******************************************************************************

SHARED Mset AS MENUSETTINGS

IF Height% < 10 THEN Height% = 10 '                                            height of menu can't be less than 10 pixels
Mset.height = Height% '                                                        set the menu height
IF (Mset.called AND 16) = 0 THEN Mset.called = Mset.called + 16 '              remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUFONT (PathToFont$, FontSize%)

'******************************************************************************
'*                                                                            *
'* Sets the font and font size for the menu system to use                     *
'*                                                                            *
'* PathToFont$  - specifies where (path) font is and its file name            *
'*                                                                            *
'* FontSize%    - specifies size of font loaded                               *
'*                                                                            *
'* Examples:      SETMENUFONT "c:\windows\fonts\lucon.ttf", 12                *
'*                SETMENUFONT "myfont.ttf", 16                                *
'*                                                                            *
'* If the font specified in PathToFont$ does not exist then the deafult       *
'* system font will be used                                                   *
'*                                                                            *
'******************************************************************************

DIM Exists% '                                                                  TRUE (-1) if font exists, FALSE (0) otherwise

SHARED Mset AS MENUSETTINGS

IF FontSize% < 1 THEN FontSize% = 1 '                                          font size must be 1 or higher
Exists% = _FILEEXISTS(PathToFont$) '                                           does the font exist?
IF Exists% = 0 THEN '                                                          was the font located?
    Mset.font = _FONT(0) '                                                     no, use default destination font
ELSE '                                                                         yes
    Mset.font = _LOADFONT(PathToFont$, FontSize%) '                            load font
END IF
IF (Mset.called AND 1) = 0 THEN Mset.called = Mset.called + 1 '                remember that this subroutine has been called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB DrawSubMenu (MainEntry%)

'******************************************************************************
'*                                                                            *
'* Draws the initial submenu images (internal library use only)               *
'*                                                                            *
'******************************************************************************

DIM lx% '                                                                      upper left  X corner of submenu
DIM ly% '                                                                      upper left  Y corner of submenu
DIM rx% '                                                                      lower right X corner of submenu
DIM ry% '                                                                      lower right Y corner of submenu
DIM Red% '                                                                     red component of submenu colors
DIM Green% '                                                                   green component of submenu colors
DIM Blue% '                                                                    blue component of submenu colors
DIM Light~& '                                                                  bright version of submenu color
DIM Dark~& '                                                                   dark version of submenu color
DIM Darker~& '                                                                 darker version of submenu color

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

lx% = 0 '                                                                      upper left X corner of submenu
ly% = 0 '                                                                      upper left Y corner of submenu
rx% = _WIDTH(Menu(MainEntry%, 0).submenu) - 1 '                                lower right X corner of submenu
ry% = _HEIGHT(Menu(MainEntry%, 0).submenu) - 1 '                               lower right Y corner of submenu
_DEST Menu(MainEntry%, 0).submenu '                                            set destination graphic as submenu mage
CLS , Mset.smabg '                                                             set submenu background color
Red% = _RED(Mset.smabg) '                                                      get red component of normal background color
Green% = _GREEN(Mset.smabg) '                                                  get green component of normal background color
Blue% = _BLUE(Mset.smabg) '                                                    get blue component of normal background color
Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2) '                             calculate a dark version of the normal background color
Darker~& = _RGB32(Red% \ 4, Green% \ 4, Blue% \ 4) '                           calculate a darker version of the normal background color
Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2) '                            calculate a lighter version of the normal background color
IF Mset.smbar3D THEN '                                                         should submenu be in 3D?
    LINE (lx% + 1, ry% - 2)-(lx% + 1, ly% + 1), Light~& '                      yes, draw a raised box look around submenu
    LINE -(rx% - 2, ly% + 1), Light~&
    LINE (lx%, ry%)-(rx%, ry%), Darker~&
    LINE -(rx%, ly%), Darker~&
    LINE (lx% + 1, ry% - 1)-(rx% - 1, ry% - 1), Dark~&
    LINE -(rx% - 1, ly% + 1), Dark~&
ELSE
    LINE (lx%, ly%)-(rx%, ry%), Dark~&, B '                                    no, draw a dark box around submenu
    LINE (lx% + 1, ly% + 1)-(rx% - 1, ry% - 1), Dark~&, B
END IF

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB DrawMenuBars ()

'******************************************************************************
'*                                                                            *
'* Draws the three menu bars Mset.menubar, Mset.menubarhighlight and          *
'* Mset.menubarselected (internal library use only)                           *
'*                                                                            *
'******************************************************************************

DIM lx% '                                                                      upper left  X corner of menu bars
DIM ly% '                                                                      upper left  Y corner of menu bars
DIM rx% '                                                                      lower right X corner of menu bars
DIM ry% '                                                                      lower right Y corner of menu bars
DIM Red% '                                                                     red component of menu bar colors
DIM Green% '                                                                   green component of menu bar colors
DIM Blue% '                                                                    blue component of menu bar colors
DIM Dark~& '                                                                   dark version of menu bar colors
DIM Light~& '                                                                  bright version of of menu bar colors

SHARED Mset AS MENUSETTINGS

lx% = 0 '                                                                      upper left X corner of menu bar
ly% = 0 '                                                                      upper left Y corner of menu bar
rx% = _WIDTH(Mset.menubar) - 1 '                                               lower right X corner of menu bar
ry% = _HEIGHT(Mset.menubar) - 1 '                                              lower right Y corner of menu bar
_DEST Mset.menubar '                                                           set normal menu bar as destination graphic
CLS , Mset.mmabarbg '                                                          set normal menu bar background color
IF Mset.mmbar3D THEN '                                                         should menu bar be in 3D?
    Red% = _RED(Mset.mmabarbg) '                                               yes, get the red component of normal menu bar background
    Green% = _GREEN(Mset.mmabarbg) '                                           get the green component of normal menu bar background
    Blue% = _BLUE(Mset.mmabarbg) '                                             get the blue component of normal menu bar background
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2) '                         calculate a dark version of normal menu bar background
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2) '                        calculate a bright verion of normal menu bar background
    LINE (lx%, ry% - 1)-(lx%, ly%), Light~& '                                  draw a raised box look around normal menu bar
    LINE -(rx% - 1, ly%), Light~&
    LINE (lx%, ry%)-(rx%, ry%), Dark~&
    LINE -(rx%, ly% + 1), Dark~&
END IF
_DEST Mset.menubarhighlight '                                                  set highlighted menu bar as destination graphic
CLS , Mset.mmabarbg
LINE (lx%, ly% + 2)-(rx%, ry% - 2), Mset.mmhbarbg, BF
IF Mset.mmbar3D THEN
    Red% = _RED(Mset.mmhbarbg)
    Green% = _GREEN(Mset.mmhbarbg)
    Blue% = _BLUE(Mset.mmhbarbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 1)-(lx%, ly%), Light~&
    LINE -(rx% - 1, ly%), Light~&
    LINE (lx%, ry%)-(rx%, ry%), Dark~&
    LINE -(rx%, ly% + 1), Dark~&
END IF
_DEST Mset.menubarselected '                                                   set selected menu bar as destination graphic
CLS , Mset.mmabarbg
LINE (lx%, ly% + 2)-(rx%, ry% - 2), Mset.mmsbarbg, BF
IF Mset.mmbar3D THEN
    Red% = _RED(Mset.mmsbarbg)
    Green% = _GREEN(Mset.mmsbarbg)
    Blue% = _BLUE(Mset.mmsbarbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 1)-(lx%, ly%), Light~&
    LINE -(rx% - 1, ly%), Light~&
    LINE (lx%, ry%)-(rx%, ry%), Dark~&
    LINE -(rx%, ly% + 1), Dark~&
END IF

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB DrawSubEntry (MainEntry%, SubEntry%)

'******************************************************************************
'*                                                                            *
'* Draw the sub menu entries (internal library use only)                      *
'*                                                                            *
'******************************************************************************

DIM lx% '                                                                      upper left X corner of submenu entry
DIM ly% '                                                                      upper left Y corner of submenu entry
DIM rx% '                                                                      lower right X corner of submenu entry
DIM ry% '                                                                      lower right Y corner of submenu entry
DIM Red% '                                                                     red component of submenu entry
DIM Green% '                                                                   green component of submenu entry
DIM Blue% '                                                                    blue component of submenu entry
DIM Light~& '                                                                  bright version of submenu color
DIM Dark~& '                                                                   dark version of submenu color
DIM sm3d%
DIM smbar3d%

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

lx% = 0 '                                                                      upper left X location of submenu entry
ly% = 0 '                                                                      upper left Y location of submenu entry
rx% = _WIDTH(Menu(MainEntry%, SubEntry%).active) - 1 '                         lower right X location of submenu entry
ry% = _HEIGHT(Menu(MainEntry%, SubEntry%).active) - 1 '                        lower right Y location of submenu entry
sm3d% = ABS(Mset.sm3D)
smbar3d% = ABS(Mset.smbar3D)
_DEST Menu(MainEntry%, SubEntry%).active '                                     set the normal submenu entry as the destination
CLS , Mset.smabg '                                                             set the normal submenu entry background color
COLOR Mset.smatext '                                                           set the normal submenu entry text color
_PRINTSTRING (lx% + Mset.spacing, Mset.centered + Mset.texttweak), RTRIM$(Menu(MainEntry%, SubEntry%).ljustify) ' print text to normal submenu entry
IF Menu(MainEntry%, SubEntry%).hotkey THEN '                                   print right justified hotkey combo if hotkey present
    _PRINTSTRING (rx% - _PRINTWIDTH(RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)) - Mset.spacing, Mset.centered + Mset.texttweak), RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)
END IF
IF Menu(MainEntry%, SubEntry%).altkey THEN '                                   draw ALT key underscore if ALT key present
    LINE (lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak)-(lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + Menu(MainEntry%, SubEntry%).altkeywidth, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak), Mset.smatext
END IF
Red% = _RED(Mset.smabg) '                                                      get red component of normal submenu entry background color
Green% = _GREEN(Mset.smabg) '                                                  get green component of normal submenu entry background color
Blue% = _BLUE(Mset.smabg) '                                                    get blue component of normal submenu entry background color
Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2) '                             calculate dark version of normal background color
Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2) '                            calculate bright version of normal background color
IF Mset.sm3D THEN '                                                            should submenu entry be in 3D?
    LINE (lx%, ry% - 1)-(lx%, ly%), Light~& '                                  yes, draw raised box look around submenu entry
    LINE -(rx% - 1, ly%), Light~&
    LINE (lx%, ry%)-(rx%, ry%), Dark~&
    LINE -(rx%, ly%), Dark~&
ELSEIF Menu(MainEntry%, SubEntry%).drawline THEN '                             no, should a line be drawn above this submenu entry?
    IF Mset.smbar3D THEN '                                                     yes, is the submenu box in 3D?
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Light~& '                          yes, draw line at the top of this entry
    ELSE
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Dark~& '                           no, draw line at the top of this entry
    END IF
    _DEST Menu(MainEntry%, SubEntry% - 1).active '                             make the submenu entry above this one the destination
    LINE (lx% + 1, ry%)-(rx% - 1, ry%), Dark~& '                               draw a line at the bottom of this entry
    '**************************************************************************
    '* Redraw the entry above since a line was drawn on it                    *
    '**************************************************************************
    _PUTIMAGE (Menu(MainEntry%, SubEntry% - 1).x, Menu(MainEntry%, SubEntry% - 1).y), Menu(MainEntry%, SubEntry% - 1).active, Menu(MainEntry%, 0).submenu
END IF
'******************************************************************************
'* draw the entry onto the normal submenu image                               *
'******************************************************************************
_PUTIMAGE (Menu(MainEntry%, SubEntry%).x, Menu(MainEntry%, SubEntry%).y), Menu(MainEntry%, SubEntry%).active, Menu(MainEntry%, 0).submenu
'******************************************************************************
'* the remainder of the code in this subroutine follows the same order as     *
'* above except that the highlight, inactive and inactive highlight menu      *
'* entries are not drawn onto the normal submenu image                        *
'******************************************************************************
_DEST Menu(MainEntry%, SubEntry%).highlight
CLS , Mset.smabg
LINE (lx% + 1, ly% + 1)-(rx% - 1, ry% - 1), Mset.smhbg, BF
COLOR Mset.smhtext
_PRINTSTRING (lx% + Mset.spacing + sm3d%, Mset.centered + Mset.texttweak + sm3d%), RTRIM$(Menu(MainEntry%, SubEntry%).ljustify)
IF Menu(MainEntry%, SubEntry%).hotkey THEN
    _PRINTSTRING (rx% - _PRINTWIDTH(RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)) - Mset.spacing + sm3d%, Mset.centered + Mset.texttweak + sm3d%), RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)
END IF
IF Menu(MainEntry%, SubEntry%).altkey THEN
    LINE (lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + sm3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + sm3d%)-(lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + Menu(MainEntry%, SubEntry%).altkeywidth + sm3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + sm3d%), Mset.smhtext
END IF
IF Mset.sm3D THEN
    Red% = _RED(Mset.smhbg)
    Green% = _GREEN(Mset.smhbg)
    Blue% = _BLUE(Mset.smhbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 1)-(lx%, ly%), Dark~&
    LINE -(rx% - 1, ly%), Dark~&
    LINE (lx%, ry%)-(rx%, ry%), Light~&
    LINE -(rx%, ly%), Light~&
ELSEIF Menu(MainEntry%, SubEntry%).drawline THEN
    Red% = _RED(Mset.smabg)
    Green% = _GREEN(Mset.smabg)
    Blue% = _BLUE(Mset.smabg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    IF Mset.smbar3D THEN
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Light~&
    ELSE
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Dark~&
    END IF
    _DEST Menu(MainEntry%, SubEntry% - 1).highlight
    LINE (lx% + 1, ry%)-(rx% - 1, ry%), Dark~&
END IF
_DEST Menu(MainEntry%, SubEntry%).inactive
CLS , Mset.smabg
LINE (lx% + 1, ly% + 1)-(rx% - 1, ry% - 1), Mset.smibg, BF
Red% = _RED(Mset.smibg)
Green% = _GREEN(Mset.smibg)
Blue% = _BLUE(Mset.smibg)
Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
IF Mset.smbar3D THEN
    COLOR Light~&
    _PRINTSTRING (lx% + Mset.spacing + 2, Mset.centered + Mset.texttweak + 2), RTRIM$(Menu(MainEntry%, SubEntry%).ljustify)
END IF
COLOR Mset.smitext
_PRINTSTRING (lx% + Mset.spacing + smbar3d%, Mset.centered + Mset.texttweak + smbar3d%), RTRIM$(Menu(MainEntry%, SubEntry%).ljustify)
IF Menu(MainEntry%, SubEntry%).hotkey THEN
    IF Mset.smbar3D THEN
        COLOR Light~&
        _PRINTSTRING (rx% - _PRINTWIDTH(RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)) - Mset.spacing + 2, Mset.centered + 2 + Mset.texttweak), RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)
    END IF
    COLOR Mset.smitext
    _PRINTSTRING (rx% - _PRINTWIDTH(RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)) - Mset.spacing + smbar3d%, Mset.centered + Mset.texttweak + smbar3d%), RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)
END IF
IF Menu(MainEntry%, SubEntry%).altkey THEN
    IF Mset.smbar3D THEN LINE (lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + 2, Mset.centered + _FONTHEIGHT(Mset.font) + 2 + Mset.alttweak + Mset.texttweak)-(lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + Menu(MainEntry%, SubEntry%).altkeywidth + 2, Mset.centered + _FONTHEIGHT(Mset.font) + 2 + Mset.alttweak + Mset.texttweak), Light~&
    LINE (lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + smbar3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + smbar3d%)-(lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + Menu(MainEntry%, SubEntry%).altkeywidth + smbar3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + smbar3d%), Mset.smitext
END IF
IF Mset.sm3D THEN
    LINE (lx%, ry% - 1)-(lx%, ly%), Dark~&
    LINE -(rx% - 1, ly%), Dark~&
    LINE (lx%, ry%)-(rx%, ry%), Light~&
    LINE -(rx%, ly%), Light~&
ELSEIF Menu(MainEntry%, SubEntry%).drawline THEN
    Red% = _RED(Mset.smabg)
    Green% = _GREEN(Mset.smabg)
    Blue% = _BLUE(Mset.smabg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    IF Mset.smbar3D THEN
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Light~&
    ELSE
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Dark~&
    END IF
    _DEST Menu(MainEntry%, SubEntry% - 1).inactive
    LINE (lx% + 1, ry%)-(rx% - 1, ry%), Dark~&
END IF
_DEST Menu(MainEntry%, SubEntry%).ihighlight
CLS , Mset.smabg
LINE (lx% + 1, ly% + 1)-(rx% - 1, ry% - 1), Mset.smihbg, BF
COLOR Mset.smihtext
_PRINTSTRING (lx% + Mset.spacing + smbar3d%, Mset.centered + Mset.texttweak + smbar3d%), RTRIM$(Menu(MainEntry%, SubEntry%).ljustify)
IF Menu(MainEntry%, SubEntry%).hotkey THEN
    _PRINTSTRING (rx% - _PRINTWIDTH(RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)) - Mset.spacing + smbar3d%, Mset.centered + Mset.texttweak + smbar3d%), RTRIM$(Menu(MainEntry%, SubEntry%).rjustify)
END IF
IF Menu(MainEntry%, SubEntry%).altkey THEN
    LINE (lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + smbar3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + smbar3d%)-(lx% + Mset.spacing + Menu(MainEntry%, SubEntry%).altkeyx + Menu(MainEntry%, SubEntry%).altkeywidth + smbar3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + smbar3d%), Mset.smihtext
END IF
IF Mset.sm3D THEN
    Red% = _RED(Mset.smihbg)
    Green% = _GREEN(Mset.smihbg)
    Blue% = _BLUE(Mset.smihbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 1)-(lx%, ly%), Dark~&
    LINE -(rx% - 1, ly%), Dark~&
    LINE (lx%, ry%)-(rx%, ry%), Light~&
    LINE -(rx%, ly%), Light~&
ELSEIF Menu(MainEntry%, SubEntry%).drawline THEN
    Red% = _RED(Mset.smabg)
    Green% = _GREEN(Mset.smabg)
    Blue% = _BLUE(Mset.smabg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    IF Mset.smbar3D THEN
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Light~&
    ELSE
        LINE (lx% + 1, ly%)-(rx% - 1, ly%), Dark~&
    END IF
    _DEST Menu(MainEntry%, SubEntry% - 1).ihighlight
    LINE (lx% + 1, ry%)-(rx% - 1, ry%), Dark~&
END IF

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB DrawMainEntry (MainEntry%)

'******************************************************************************
'*                                                                            *
'* Draw the main menu entries (internal library use only)                     *
'*                                                                            *
'******************************************************************************

DIM lx% '                                                                      upper left X corner of main menu entry
DIM ly% '                                                                      upper left Y corner of main menu entry
DIM rx% '                                                                      lower right X corner of main menu entry
DIM ry% '                                                                      lower right Y corner of main menu entry
DIM Red% '
DIM Green%
DIM Blue%
DIM Light~&
DIM Dark~&
DIM mm3d%

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

lx% = Menu(MainEntry%, 0).x
ly% = 0
rx% = Menu(MainEntry%, 0).x + Menu(MainEntry%, 0).width - 1
ry% = Mset.height - 1
mm3d% = ABS(Mset.mm3D)
_DEST Mset.menubar
COLOR Mset.mmatext
_PRINTSTRING (Menu(MainEntry%, 0).x + Mset.spacing, Mset.centered + Mset.texttweak), RTRIM$(Menu(MainEntry%, 0).text)
IF Menu(MainEntry%, 0).altkey THEN
    LINE (lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak)-(lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx + Menu(MainEntry%, 0).altkeywidth, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak), Mset.mmatext
END IF
_PUTIMAGE (0, 0), Mset.menubar, Menu(MainEntry%, 0).active, (lx%, ly%)-(rx%, ry%)
_DEST Mset.menubarhighlight
COLOR Mset.mmhtext
_PRINTSTRING (Menu(MainEntry%, 0).x + Mset.spacing, Mset.centered + Mset.texttweak), RTRIM$(Menu(MainEntry%, 0).text)
IF Menu(MainEntry%, 0).altkey THEN
    LINE (lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak)-(lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx + Menu(MainEntry%, 0).altkeywidth, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak), Mset.mmhtext
END IF
IF Mset.mm3D THEN
    Red% = _RED(Mset.mmhbarbg)
    Green% = _GREEN(Mset.mmhbarbg)
    Blue% = _BLUE(Mset.mmhbarbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 2)-(lx%, ly% + 1), Light~&
    LINE -(rx% - 1, ly% + 1), Light~&
    LINE (lx%, ry% - 1)-(rx%, ry% - 1), Dark~&
    LINE -(rx%, ly% + 1), Dark~&
END IF
_PUTIMAGE (0, 0), Mset.menubarhighlight, Menu(MainEntry%, 0).highlight, (lx%, ly%)-(rx%, ry%)
_DEST Mset.menubarselected
COLOR Mset.mmstext
_PRINTSTRING (Menu(MainEntry%, 0).x + Mset.spacing + mm3d%, Mset.centered + Mset.texttweak + mm3d%), RTRIM$(Menu(MainEntry%, 0).text)
IF Menu(MainEntry%, 0).altkey THEN
    LINE (lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx + mm3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + mm3d%)-(lx% + Mset.spacing + Menu(MainEntry%, 0).altkeyx + Menu(MainEntry%, 0).altkeywidth + mm3d%, Mset.centered + _FONTHEIGHT(Mset.font) + Mset.alttweak + Mset.texttweak + mm3d%), Mset.mmstext
END IF
IF Mset.mm3D THEN
    Red% = _RED(Mset.mmsbarbg)
    Green% = _GREEN(Mset.mmsbarbg)
    Blue% = _BLUE(Mset.mmsbarbg)
    Dark~& = _RGB32(Red% \ 2, Green% \ 2, Blue% \ 2)
    Light~& = _RGB32(Red% * 2, Green% * 2, Blue% * 2)
    LINE (lx%, ry% - 2)-(lx%, ly% + 1), Dark~&
    LINE -(rx% - 1, ly% + 1), Dark~&
    LINE (lx%, ry% - 1)-(rx%, ry% - 1), Light~&
    LINE -(rx%, ly% + 1), Light~&
END IF
_PUTIMAGE (0, 0), Mset.menubarselected, Menu(MainEntry%, 0).selected, (lx%, ly%)-(rx%, ry%)

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

SUB SETMENUSTATE (idnum%, behavior%)

'******************************************************************************
'*                                                                            *
'* Enables or disables a sub menu entry                                       *
'*                                                                            *
'* idnum%    - sub menu entry to enable or disable                            *
'*                                                                            *
'* behavior% -  0 (FALSE) disable the sub menu entry                          *
'*             -1 (TRUE)  enable the sub menu entry                           *
'*                                                                            *
'* Example:    SETMENUSTATE 201, 0                                            *
'*                                                                            *
'******************************************************************************

SHARED Menu() AS MENU

Menu(idnum% \ 100, idnum% - ((idnum% \ 100) * 100)).live = behavior% '         set submenu entry to enabled/disabled

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION CHECKMENU% (KBDCheck%) STATIC

'******************************************************************************
'*                                                                            *
'* Checks the menu for user mouse or keyboard interaction                     *
'*                                                                            *
'* Once the _KEYDOWN bug is corrected this function can be made so that       *
'* the keyboard routines are only entered once an ALT key has been detected.  *
'* Right now, the keyboard routine must be entered every time unless the      *
'* programmer overrides it with KBDCheck%.                                    *
'*                                                                            *
'* Usage: EntrySelected% = CHECKMENU%(KeyboardCheck%)                         *
'*                                                                            *
'* KeyboardCheck% - used to enable or disable keyboard menu checking          *
'*                  -1 (TRUE)  will check for keyboard/menu interaction       *
'*                   0 (FALSE) will disable keyboard menu checking            *
'*                                                                            *
'* EntrySelected% - the menu entry selected in the form of mmss               *
'*                  mm - (100 - 9900) the main menu entry is under            *
'*                  ss - (1 - 99) the submenu entry selected                  *
'*                                                                            *
'* For example, if Exit is the 5th entry under the File menu, and the File    *
'* menu is the first main menu entry, selecting Exit would return the value   *
'* 105 (100 = first main menu entry, 05 = 5th submenu entry)                  *
'*                                                                            *
'******************************************************************************

DIM Mousex% '                                                                  current  mouse X position
DIM Mousey% '                                                                  current  mouse Y position
DIM OldMousex% '                                                               previous mouse X position
DIM OldMousey% '                                                               previous mouse Y position
DIM Mbutton% '                                                                 status of left mouse button

SHARED Mset AS MENUSETTINGS

IF Mset.showing THEN
    WHILE _MOUSEINPUT: WEND '                                                  update mouse data
    Mousex% = _MOUSEX '                                                        get current mouse X position
    Mousey% = _MOUSEY '                                                        get current mouse Y position
    Mbutton% = _MOUSEBUTTON(1) '                                               get status of left mouse button
    IF (Mousex% <> OldMousex%) OR (Mousey% <> OldMousey%) OR Mbutton% THEN '   has mouse moved or left button clicked?
        CHECKMENU% = MenuMouseCheck%(Mousex%, Mousey%, Mbutton%) '             yes, report back what user did in menu
        OldMousex% = Mousex% '                                                 save as previous mouse X position
        OldMousey% = Mousey% '                                                 save as precious mouse Y position
        IF Mbutton% THEN '                                                     was left mouse button pressed?
            DO: WHILE _MOUSEINPUT: WEND: LOOP UNTIL _MOUSEBUTTON(1) = 0 '      yes, wait until it's released
        END IF
    ELSE '                                                                     there was no mouse activity
        IF KBDCheck% THEN '                                                    should keyboard activity be checked?
            CHECKMENU% = MenuKeyboardCheck% '                                  yes, return any keyboard activity
        END IF
    END IF
END IF

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION MenuKeyboardCheck% () STATIC

'******************************************************************************
'*                                                                            *
'* Interprets keystrokes related to menu interaction (internal library use)   *
'*                                                                            *
'* Returns the sub menu ID number of an entry that had ENTER pressed on it or *
'* if its hot-key combination was pressed. Returns 0 if no sub menu entry     *
'* selected.                                                                  *
'*                                                                            *
'******************************************************************************

DIM KeyPress& '              current key being pressed
DIM OldKeyPress& '           previous key that was pressed (STATIC)
DIM AltKeyDown% '            TRUE if ALT key in down position (STATIC)
DIM OldAltKeyDown% '         TRUE if ALT key was previously in down position (STATIC)
DIM KeysProcessed% '         TRUE is keys followed the ALT key (STATIC)
DIM MenuScan% '              counter used to scan through menu entries
DIM SubmenuScan% '           counter used to scan through sub menu entries
DIM HotkeyCombo& '           keeps track of hot-key combos pressed (STATIC)
DIM SubMenuSkip% '           used to skip submenus that have had all entries disabled

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

MenuKeyboardCheck% = 0
KeyPress& = _KEYHIT
DO WHILE KeyPress& <> 0
    SELECT CASE KeyPress&
        CASE 100307, 100308, -100307, -100308 '                                ** Right ALT key, Left ALT key, -Right ALT key, -Left ALT key
            '***********************************
            '* ALT key was pressed or released *
            '***********************************
            AltKeyDown% = -1 - AltKeyDown%
            IF (AltKeyDown% = 0) AND (OldAltKeyDown% = -1) AND (KeysProcessed% = 0) THEN
                '*************************************
                '* ALT key was pressed then released *
                '*************************************
                Mset.menuactive = -1 - Mset.menuactive
                IF Mset.menuactive THEN
                    Mset.mainmenu = 1
                    Mset.oldmainmenu = 1
                    _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).highlight
                ELSE
                    IF Mset.submenuactive THEN _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).undersubmenu
                    IF Mset.mainmenu THEN _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).active
                    Mset.submenu = 0
                    Mset.oldsubmenu = 0
                    Mset.submenuactive = 0
                END IF
            END IF
            OldAltKeyDown% = AltKeyDown%
            KeysProcessed% = 0
        CASE 19712, 19200 '                                                    ** Right Arrow Key, Left Arrow Key
            '*********************************************
            '* RIGHT ARROW or LEFT ARROW key was pressed *
            '*********************************************
            IF Mset.menuactive THEN
                IF KeyPress& = 19712 THEN '                                    ** Right Arrow Key
                    Mset.mainmenu = Mset.mainmenu + 1
                    IF Mset.mainmenu > UBOUND(menu) THEN Mset.mainmenu = 1
                ELSE
                    Mset.mainmenu = Mset.mainmenu - 1
                    IF Mset.mainmenu < 1 THEN Mset.mainmenu = UBOUND(menu)
                END IF
                IF Mset.submenuactive = 0 THEN
                    '*****************************************************
                    '* no submenu - just move highlight on main menu bar *
                    '*****************************************************
                    IF Mset.oldmainmenu THEN _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
                    IF Mset.mainmenu THEN _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).highlight
                ELSE
                    '************************************************************
                    '* submenu showing - move selected bar and show new submenu *
                    '************************************************************
                    IF Mset.oldmainmenu THEN
                        _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.oldmainmenu, 0).undersubmenu
                        _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
                    END IF
                    IF Mset.mainmenu THEN
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).selected
                        _PUTIMAGE (0, 0), _DEST, Menu(Mset.mainmenu, 0).undersubmenu, (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1)
                        IF Mset.shadow THEN LINE (Menu(Mset.mainmenu, 0).x + Mset.shadow, Mset.height + Mset.subtweak + Mset.shadow)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1), _RGBA32(0, 0, 0, 63), BF
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).submenu
                        FOR SubmenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                            IF Menu(Mset.mainmenu, SubmenuScan%).live = 0 THEN
                                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, SubmenuScan%).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubmenuScan%).y), Menu(Mset.mainmenu, SubmenuScan%).inactive
                            END IF
                        NEXT SubmenuScan%
                    END IF
                    Mset.submenu = 0
                    Mset.oldsubmenu = 0
                END IF
                Mset.oldmainmenu = Mset.mainmenu
                KeysProcessed% = -1
            END IF
        CASE 20480, 18432, 13, 27 '                                            ** Down Arrow Key, Up Arrow Key, ENTER Key, ESCape Key
            '******************************************************
            '* DOWN ARROW, UP ARROW, ENTER or ESC key was pressed *
            '******************************************************
            IF Mset.menuactive THEN
                IF (Mset.submenuactive = 0) AND (KeyPress& <> 27) THEN '           ** ESCape Key
                    '*************************************
                    '* no submenu showing - show submenu *
                    '*************************************
                    IF Mset.mainmenu THEN
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).selected
                        _PUTIMAGE (0, 0), _DEST, Menu(Mset.mainmenu, 0).undersubmenu, (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1)
                        IF Mset.shadow THEN LINE (Menu(Mset.mainmenu, 0).x + Mset.shadow, Mset.height + Mset.subtweak + Mset.shadow)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1), _RGBA32(0, 0, 0, 63), BF
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).submenu
                        FOR SubmenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                            IF Menu(Mset.mainmenu, SubmenuScan%).live = 0 THEN
                                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, SubmenuScan%).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubmenuScan%).y), Menu(Mset.mainmenu, SubmenuScan%).inactive
                            END IF
                        NEXT SubmenuScan%
                    END IF
                    Mset.submenu = 0
                    Mset.oldsubmenu = 0
                    Mset.submenuactive = -1
                ELSE
                    '*******************
                    '* submenu showing *
                    '*******************
                    IF KeyPress& = 20480 OR KeyPress& = 18432 THEN '           ** Down Arrow Key, Up Arrow Key
                        '*****************************************************
                        '* DOWN ARROW key pressed, move down submenu entries *
                        '*****************************************************
                        IF KeyPress& = 20480 THEN '                            ** Down Arrow Key
                            SubMenuSkip% = Mset.submenu
                            DO
                                Mset.submenu = Mset.submenu + 1
                                IF Mset.submenu > Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100) THEN Mset.submenu = 1
                            LOOP UNTIL (Mset.submenu = SubMenuSkip%) OR Menu(Mset.mainmenu, Mset.submenu).live
                        ELSE
                            SubMenuSkip% = Mset.submenu
                            DO
                                Mset.submenu = Mset.submenu - 1
                                IF Mset.submenu < 1 THEN Mset.submenu = Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                            LOOP UNTIL (Mset.submenu = SubMenuSkip%) OR Menu(Mset.mainmenu, Mset.submenu).live
                        END IF
                        IF Mset.oldsubmenu THEN
                            IF Menu(Mset.mainmenu, Mset.oldsubmenu).live THEN
                                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.oldsubmenu).y), Menu(Mset.mainmenu, Mset.oldsubmenu).active
                            ELSE
                                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.oldsubmenu).y), Menu(Mset.mainmenu, Mset.oldsubmenu).inactive
                            END IF
                        END IF
                        IF Menu(Mset.mainmenu, Mset.submenu).live THEN
                            _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.submenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.submenu).y), Menu(Mset.mainmenu, Mset.submenu).highlight
                        ELSE
                            _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.submenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.submenu).y), Menu(Mset.mainmenu, Mset.submenu).inactive
                        END IF
                        Mset.oldsubmenu = Mset.submenu
                    ELSE
                        '**********************************************************************
                        '* ENTER or ESC key pressed - return ID number of submenu or 0 if ESC *
                        '**********************************************************************
                        IF Mset.submenu > 0 THEN
                            IF KeyPress& = 13 THEN '                           ** ENTER Key
                                MenuKeyboardCheck% = (Menu(Mset.mainmenu, 0).idnum \ 100) * 100 + Menu(Mset.mainmenu, Mset.submenu).idnum
                            ELSE
                                MenuKeyboardCheck% = 0
                            END IF
                        END IF
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).undersubmenu
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).active
                        Mset.submenu = 0
                        Mset.oldsubmenu = 0
                        Mset.mainmenu = 0
                        Mset.oldmainmenu = 0
                        Mset.submenuactive = 0
                        Mset.menuactive = 0
                        EXIT FUNCTION
                    END IF
                END IF
                KeysProcessed% = -1
            END IF
        CASE ELSE
            '**********************************
            '* find out which key was pressed *
            '**********************************
            IF ABS(KeyPress&) >= 97 AND ABS(KeyPress&) <= 122 THEN KeyPress& = KeyPress& - SGN(KeyPress&) * 32 ' uppercase all alpha keys
            IF AltKeyDown% AND (Mset.submenuactive = 0) THEN
                '******************************************************
                '* scan through list of altkeycharacters in main menu *
                '******************************************************
                FOR MenuScan% = 1 TO UBOUND(menu)
                    IF KeyPress& = Menu(MenuScan%, 0).altkeycharacter THEN
                        Mset.mainmenu = MenuScan%
                        Mset.menuactive = -1
                        '****************************************
                        '* show submenu that has been activated *
                        '****************************************
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).selected
                        _PUTIMAGE (0, 0), _DEST, Menu(Mset.mainmenu, 0).undersubmenu, (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1)
                        IF Mset.shadow THEN LINE (Menu(Mset.mainmenu, 0).x + Mset.shadow, Mset.height + Mset.subtweak + Mset.shadow)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1), _RGBA32(0, 0, 0, 63), BF
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).submenu
                        Mset.oldmainmenu = Mset.mainmenu
                        Mset.submenu = 0
                        Mset.oldsubmenu = 0
                        Mset.submenuactive = -1
                        EXIT FOR
                    END IF
                NEXT MenuScan%
            ELSEIF Mset.submenuactive THEN
                '*******************************************************
                '* scan through list of alt key characters in sub menu *
                '*******************************************************
                FOR MenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                    IF KeyPress& = Menu(Mset.mainmenu, MenuScan%).altkeycharacter THEN
                        MenuKeyboardCheck% = (Menu(Mset.mainmenu, 0).idnum \ 100) * 100 + Menu(Mset.mainmenu, MenuScan%).idnum
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).undersubmenu
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).active
                        Mset.submenu = 0
                        Mset.oldsubmenu = 0
                        Mset.mainmenu = 0
                        Mset.oldmainmenu = 0
                        Mset.submenuactive = 0
                        Mset.menuactive = 0
                        EXIT FUNCTION
                    END IF
                NEXT MenuScan%
            ELSE
                '******************************************************
                '* make both CTRL and SHIFT keys equal the same value *
                '******************************************************
                IF KeyPress& <> OldKeyPress& THEN
                    IF (ABS(KeyPress&) = 100306) OR (ABS(KeyPress&) = 100304) THEN KeyPress& = KeyPress& - SGN(KeyPress&) ' ** Left CTRL Key, Left SHIFT Key
                    HotkeyCombo& = HotkeyCombo& + KeyPress&
                END IF
                OldKeyPress& = KeyPress&
                IF HotkeyCombo& < 0 THEN HotkeyCombo& = 0 ' discard released key values
                '*********************************************
                '* scan list of hotkeys through all submenus *
                '*********************************************
                IF HotkeyCombo& > 0 THEN
                    FOR MenuScan% = 1 TO UBOUND(menu)
                        FOR SubmenuScan% = 1 TO Menu(MenuScan%, 0).idnum - ((Menu(MenuScan%, 0).idnum \ 100) * 100)
                            IF HotkeyCombo& = Menu(MenuScan%, SubmenuScan%).hotkey THEN
                                MenuKeyboardCheck% = (Menu(MenuScan%, 0).idnum \ 100) * 100 + Menu(MenuScan%, SubmenuScan%).idnum
                                EXIT FUNCTION
                            END IF
                        NEXT SubmenuScan%
                    NEXT MenuScan%
                END IF
            END IF
            KeysProcessed% = -1
    END SELECT
    KeyPress& = _KEYHIT
LOOP

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

FUNCTION MenuMouseCheck% (Mx%, My%, Mb%)

'******************************************************************************
'*                                                                            *
'* Interprets mouse actions related to menu interaction (internal library use)*
'*                                                                            *
'* Returns the sub menu ID number of an entry that had the left mouse button  *
'* clicked on it or if its hot-key combination was pressed. Returns 0 if no   *
'* sub menu entry was selected.                                               *
'*                                                                            *
'******************************************************************************

DIM MainMenuScan% '        counter used to scan main menu entries
DIM SubMenuScan% '         counter used to scan submenu entries

SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

MenuMouseCheck% = 0
IF (Mx% > Mset.indent) AND (Mx% < Mset.width + Mset.indent) AND (My% > 2) AND (My% < Mset.height - 2) THEN
    Mset.menuactive = -1
    Mset.mainmenu = 0
    FOR MainMenuScan% = 1 TO UBOUND(menu)
        IF (Mx% >= Menu(MainMenuScan%, 0).x) AND (Mx% <= Menu(MainMenuScan%, 0).x + Menu(MainMenuScan%, 0).width - 1) THEN Mset.mainmenu = MainMenuScan%
    NEXT MainMenuScan%
    IF Mset.mainmenu <> Mset.oldmainmenu THEN '                                         ** mouse has changed menu position
        IF Mset.submenuactive THEN '                                                ** submenu is showing
            '*********************************************
            '* turn off old submenu and show new submenu *
            '*********************************************
            IF Mset.oldmainmenu THEN
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.oldmainmenu, 0).undersubmenu
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
            END IF
            IF Mset.mainmenu THEN
                _PUTIMAGE (0, 0), _DEST, Menu(Mset.mainmenu, 0).undersubmenu, (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1)
                IF Mset.shadow THEN LINE (Menu(Mset.mainmenu, 0).x + Mset.shadow, Mset.height + Mset.subtweak + Mset.shadow)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1), _RGBA32(0, 0, 0, 63), BF
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).selected
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).submenu
                FOR SubMenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                    IF Menu(Mset.mainmenu, SubMenuScan%).live = 0 THEN
                        _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, SubMenuScan%).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubMenuScan%).y), Menu(Mset.mainmenu, SubMenuScan%).inactive
                    END IF
                NEXT SubMenuScan%
            END IF
            Mset.submenu = 0
            Mset.oldsubmenu = 0
        ELSE '                                                                 ** submenu is not showing
            '*************************************
            '* switch to new main menu highlight *
            '*************************************
            IF Mset.oldmainmenu THEN _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
            IF Mset.mainmenu THEN _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).highlight
        END IF
        Mset.oldmainmenu = Mset.mainmenu
    ELSEIF Mb% THEN
        '**************************************************
        '* clicked on main menu entry to bring up submenu *
        '**************************************************
        Mset.submenuactive = -1 - Mset.submenuactive
        IF Mset.submenuactive THEN
            _PUTIMAGE (0, 0), _DEST, Menu(Mset.mainmenu, 0).undersubmenu, (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1)
            IF Mset.shadow THEN LINE (Menu(Mset.mainmenu, 0).x + Mset.shadow, Mset.height + Mset.subtweak + Mset.shadow)-(Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).undersubmenu) - 1, Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).undersubmenu) - 1), _RGBA32(0, 0, 0, 63), BF
            _PUTIMAGE (Menu(Mset.mainmenu, 0).x, 0), Menu(Mset.mainmenu, 0).selected
            _PUTIMAGE (Menu(Mset.mainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.mainmenu, 0).submenu
            FOR SubMenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
                IF Menu(Mset.mainmenu, SubMenuScan%).live = 0 THEN
                    _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, SubMenuScan%).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubMenuScan%).y), Menu(Mset.mainmenu, SubMenuScan%).inactive
                END IF
            NEXT SubMenuScan%
        ELSE
            _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.oldmainmenu, 0).undersubmenu
            _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).highlight
        END IF
    ELSEIF Mset.submenuactive THEN
        '**************************************
        '* turn off highlighted submenu entry *
        '**************************************
        IF Mset.oldsubmenu THEN
            IF Menu(Mset.oldmainmenu, Mset.oldsubmenu).live THEN
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x + Menu(Mset.oldmainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.oldmainmenu, Mset.oldsubmenu).y), Menu(Mset.oldmainmenu, Mset.oldsubmenu).active
            ELSE
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x + Menu(Mset.oldmainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.oldmainmenu, Mset.oldsubmenu).y), Menu(Mset.oldmainmenu, Mset.oldsubmenu).inactive
            END IF
        END IF
        Mset.submenu = 0
        Mset.oldsubmenu = 0
    END IF
ELSEIF Mset.submenuactive AND (Mx% >= Menu(Mset.mainmenu, 0).x) AND (Mx% <= Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).submenu) - 1) AND (My% >= Mset.height + Mset.subtweak) AND (My% <= Mset.height + Mset.subtweak + _HEIGHT(Menu(Mset.mainmenu, 0).submenu) - 1) THEN
    '**********************************************************
    '* there is a dropdown submenu showing and mouse is on it *
    '**********************************************************
    FOR SubMenuScan% = 1 TO Menu(Mset.mainmenu, 0).idnum - ((Menu(Mset.mainmenu, 0).idnum \ 100) * 100)
        IF (Mx% >= Menu(Mset.mainmenu, 0).x) AND (Mx% <= Menu(Mset.mainmenu, 0).x + _WIDTH(Menu(Mset.mainmenu, 0).submenu) - 1) AND (My% >= Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubMenuScan%).y) AND (My% <= Mset.height + Mset.subtweak + Menu(Mset.mainmenu, SubMenuScan%).y + Mset.height - 1) THEN
            Mset.submenu = SubMenuScan%
            EXIT FOR
        END IF
    NEXT SubMenuScan%
    IF Mset.submenu <> Mset.oldsubmenu THEN
        '*******************************************************
        '* mouse has changed from one submenu entry to another *
        '*******************************************************
        IF Mset.oldsubmenu THEN
            IF Menu(Mset.mainmenu, Mset.oldsubmenu).live THEN
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.oldsubmenu).y), Menu(Mset.mainmenu, Mset.oldsubmenu).active
            ELSE
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.oldsubmenu).y), Menu(Mset.mainmenu, Mset.oldsubmenu).inactive
            END IF
        END IF
        IF Mset.submenu THEN
            IF Menu(Mset.mainmenu, Mset.submenu).live THEN
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.submenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.submenu).y), Menu(Mset.mainmenu, Mset.submenu).highlight
            ELSE
                _PUTIMAGE (Menu(Mset.mainmenu, 0).x + Menu(Mset.mainmenu, Mset.submenu).x, Mset.height + Mset.subtweak + Menu(Mset.mainmenu, Mset.submenu).y), Menu(Mset.mainmenu, Mset.submenu).ihighlight
            END IF
        END IF
        Mset.oldsubmenu = Mset.submenu
    ELSEIF Mb% THEN
        '**************************************************
        '* mouse has been clicked - return menu ID number *
        '**************************************************
        IF Menu(Mset.oldmainmenu, Mset.submenu).live THEN
            _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.oldmainmenu, 0).undersubmenu
            _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
            MenuMouseCheck% = (Menu(Mset.mainmenu, 0).idnum \ 100) * 100 + Menu(Mset.mainmenu, Mset.submenu).idnum
            Mset.mainmenu = 0
            Mset.oldmainmenu = 0
            Mset.submenu = 0
            Mset.oldsubmenu = 0
            Mset.menuactive = 0
            Mset.submenuactive = 0
            EXIT FUNCTION
        END IF
    END IF
ELSE '                                                                         ** mouse not on submenu
    '**************************************
    '* turn off highlighted submenu entry *
    '**************************************
    IF Mset.submenuactive THEN
        IF Mset.oldsubmenu THEN
            IF Menu(Mset.oldmainmenu, Mset.oldsubmenu).live THEN
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x + Menu(Mset.oldmainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.oldmainmenu, Mset.oldsubmenu).y), Menu(Mset.oldmainmenu, Mset.oldsubmenu).active
            ELSE
                _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x + Menu(Mset.oldmainmenu, Mset.oldsubmenu).x, Mset.height + Mset.subtweak + Menu(Mset.oldmainmenu, Mset.oldsubmenu).y), Menu(Mset.oldmainmenu, Mset.oldsubmenu).inactive
            END IF
        END IF
        Mset.submenu = 0
        Mset.oldsubmenu = 0
    ELSE
        IF Mset.oldmainmenu THEN
            _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
            Mset.mainmenu = 0
            Mset.oldmainmenu = 0
            Mset.menuactive = 0
        END IF
    END IF
    '*****************************************
    '* button clicked off entire menu system *
    '*****************************************
    IF Mb% THEN
        IF Mset.submenuactive THEN _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, Mset.height + Mset.subtweak), Menu(Mset.oldmainmenu, 0).undersubmenu
        IF Mset.oldmainmenu THEN _PUTIMAGE (Menu(Mset.oldmainmenu, 0).x, 0), Menu(Mset.oldmainmenu, 0).active
        Mset.mainmenu = 0
        Mset.oldmainmenu = 0
        Mset.submenu = 0
        Mset.oldsubmenu = 0
        Mset.menuactive = 0
        Mset.submenuactive = 0
    END IF
END IF

END FUNCTION

'--------------------------------------------------------------------------------------------------------------------------------

SUB MAKEMENU ()

'******************************************************************************
'* Creates the menu graphics for the menu system.
'*
'* Structure of menu entries:
'*
'* Menu entries must be put into DATA statements. For example:
'*
'* DATA "&File","&New#Ctrl+N","&Open#Ctrl+O","-&Save#Ctrl+S"
'* DATA "Save &as...#Ctrl+Shift+S","-E&xit#Ctrl+Q","*","!"
'*
'* Would create a new main menu entry called "File" and a submenu containing
'* "New", "Open", Save", "Save as..." and "Exit" appearing like so:
'*
'* +----+
'* |File|                        ID = 105 (1xx = first menu entry  )
'* |~   |                                 (x05 = 5 sub menu entries)
'* +----+--------------------+
'* | New              Ctrl+N |   ID = 1 (returned as 101)
'* | ~                       |
'* | Open             Ctrl+O |   ID = 2 (returned as 102)
'* | ~                       |                .
'* | ------------------------|                .
'* | Save             Ctrl+S |   ID = 3       .
'* | ~                       |                .
'* | Save as... Ctrl+Shift+S |   ID = 4       .
'* |      ~                  |                .
'* | ------------------------|                .
'* | Exit             Ctrl+Q |   ID = 5 (returned as 105)
'* |  ~                      |
'* +-------------------------+
'*
'* Symbols used:
'*
'* "-" - placing a dash at beginning places a line above this entry
'* "#" - text will be right justified
'* "&" - the following letter is the ALT key combination (underline)
'* "*" - denotes end of submenu entries   (must be alone)
'* "!" - denotes end of main menu entries (must be alone)
'*
'* Hotkey combinations following the "#" symbol will automatically be
'* deciphered, but the only key seperators allowed are the "+" plus symbol
'* and " " space. The following are valid:
'*
'* #Ctrl+X   #CTRLX   #Ctrl X   #ctrlx   #ctrl+x   #CTRL+ X   etc..
'*
'* The following names (upper, lower or any combination of case) can be used
'* for special keys:
'*
'* F1, F2 ... F12, CTRL, SHIFT, DEL, DELETE, INS, INSERT, HOME, END
'* PGUP, PAGEUP, PAGE UP, PGDN, PAGEDOWN, PAGE DOWN, UP, UPARROW, UP ARROW
'* DOWN, DOWNARROW, DOWN ARROW, RIGHT, RIGHTARROW, RIGHT ARROW, LEFT
'* LEFTARROW, LEFT ARROW, SCROLL, SLOCK, SCROLLLOCK, SCROLL LOCK, CAPS
'* CLOCK, CAPSLOCK, CAPS LOCK, NLOCK, NUMLOCK, NUM LOCK
'*
'******************************************************************************

DIM Count%
DIM x%
DIM MainMenu%
DIM SubMenu%
DIM MaxSubMenu%
DIM Selected$
DIM Length1%
DIM Length2%
DIM MaxLength1%
DIM MaxLength2%
DIM ReadData$
DIM Plus%
DIM Space%
DIM Rjustify$
DIM ScreenWidth%
DIM FontHold&
DIM DestHold&

REDIM MaxLength%(0)
REDIM TotalSubmenu%(0)
SHARED Menu() AS MENU
SHARED Mset AS MENUSETTINGS

'******************************************************************************
'* Set default values if not set by programmer. These defaults will set up a  *
'* generic menu with the standard Windows look and feel to it.                *
'******************************************************************************
IF (Mset.called AND 1) = 0 THEN Mset.font = _FONT(0)
IF (Mset.called AND 2) = 0 THEN SETMAINMENUCOLORS _RGB32(0, 0, 0), _RGB32(0, 0, 0), _RGB32(0, 0, 0), _RGB32(212, 208, 200), _RGB32(212, 208, 200), _RGB32(212, 208, 200)
IF (Mset.called AND 4) = 0 THEN
    IF Mset.sm3D THEN
        SETSUBMENUCOLORS _RGB32(0, 0, 0), _RGB32(0, 0, 0), _RGB32(128, 128, 128), _RGB32(128, 128, 128), _RGB32(212, 208, 200), _RGB32(212, 208, 200), _RGB32(212, 208, 200), _RGB32(212, 208, 200)
    ELSE
        SETSUBMENUCOLORS _RGB32(0, 0, 0), _RGB32(255, 255, 255), _RGB32(128, 128, 128), _RGB32(128, 128, 128), _RGB32(212, 208, 200), _RGB32(10, 36, 106), _RGB32(212, 208, 200), _RGB32(10, 36, 106)
    END IF
END IF
IF (Mset.called AND 8) = 0 THEN SETMENU3D 0, -1, -1, 0
IF (Mset.called AND 16) = 0 THEN SETMENUHEIGHT _FONTHEIGHT(Mset.font) + 8
IF (Mset.called AND 32) = 0 THEN SETMENUSPACING 10
IF (Mset.called AND 64) = 0 THEN SETMENUINDENT 0
IF (Mset.called AND 128) = 0 THEN SETMENUTEXT 1
IF (Mset.called AND 256) = 0 THEN SETMENUUNDERSCORE -1
IF (Mset.called AND 512) = 0 THEN SETSUBMENULOCATION 0
IF (Mset.called AND 1024) = 0 THEN SETMENUSHADOW 5

ScreenWidth% = _WIDTH(_DEST) '                                                 get the width of the current screen
DestHold& = _DEST '                                                            remember the calling destination
FontHold& = _FONT(_DEST) '                                                     remember the font used when entering this routine
_FONT Mset.font '                                                              set the font used for menu entries

'******************************************************************************
'* Scan the menu DATA to get menu and submenu sizing to create menu array     *
'******************************************************************************
MainMenu% = 0 '                                                                reset main menu entry counter
MaxSubMenu% = 0 '                                                              reset maximum submenus seen
READ ReadData$ '                                                               first entry must be main menu entry
DO WHILE ReadData$ <> "!" '                                                    stop looping when encountered
    MainMenu% = MainMenu% + 1 '                                                increment main menu entry counter
    READ ReadData$ '                                                           second entry must be sub menu entry
    SubMenu% = 0 '                                                             reset sub menu entry counter
    MaxLength1% = 0 '                                                          reset max length of left text seen
    MaxLength2% = 0 '                                                          reset max length right justified text seen
    DO WHILE ReadData$ <> "*" '                                                stop looping when encountered
        SubMenu% = SubMenu% + 1 '                                              increment sub menu entry counter
        Length1% = 0 '                                                         reset length of left text seen
        Length2% = 0 '                                                         reset length of right justified text seen
        '**********************************************************************
        '* strip away all special formatting characters to reveal text only   *
        '**********************************************************************
        IF INSTR(ReadData$, "-") THEN ReadData$ = RIGHT$(ReadData$, LEN(ReadData$) - 1)
        IF INSTR(ReadData$, "&") THEN ReadData$ = LEFT$(ReadData$, INSTR(ReadData$, "&") - 1) + RIGHT$(ReadData$, LEN(ReadData$) - INSTR(ReadData$, "&"))
        IF INSTR(ReadData$, "#") THEN
            '******************************************************************
            '* get the length of left and right justified text and spacing    *
            '******************************************************************
            Length1% = _PRINTWIDTH(LEFT$(ReadData$, INSTR(ReadData$, "#") - 1)) + Mset.spacing * 2
            Length2% = _PRINTWIDTH(RIGHT$(ReadData$, LEN(ReadData$) - INSTR(ReadData$, "#"))) + Mset.spacing
        ELSE
            Length1% = _PRINTWIDTH(ReadData$) + Mset.spacing * 2
        END IF
        IF Length1% > MaxLength1% THEN MaxLength1% = Length1% '                remember the maximum left text seen
        IF Length2% > MaxLength2% THEN MaxLength2% = Length2% '                remember the maximum right justified text seen
        READ ReadData$ '                                                       read next sub menu entry
    LOOP
    IF SubMenu% > MaxSubMenu% THEN MaxSubMenu% = SubMenu% '                    save the largest number of sub menu entries seen
    REDIM _PRESERVE MaxLength%(MainMenu%) '                                    increase submenu max length array
    MaxLength%(MainMenu%) = MaxLength1% + MaxLength2% '                        save maximum submenu entry length for this submenu
    REDIM _PRESERVE TotalSubmenu%(MainMenu%) '                                 increase submenu entry counter
    TotalSubmenu%(MainMenu%) = SubMenu% '                                      save nnumber of submenu entries seen for this submenu
    READ ReadData$ '                                                           read next main menu entry
LOOP
REDIM Menu(MainMenu%, MaxSubMenu%) AS MENU '                                   resize the menu entry array accordingly
RESTORE '                                                                      restore the DATA to be read in again
x% = Mset.indent
MainMenu% = 0 '                                                                reset main menu entry counter
SubMenu% = 0 '                                                                 reset submenu entry counter
Mset.centered = (Mset.height - _FONTHEIGHT(Mset.font)) \ 2 - 1 '               centered location of text on main/submenu entries
READ ReadData$ '                                                               first entry must be main menu entry
DO WHILE ReadData$ <> "!" '                                                    stop looping when encountered
    MainMenu% = MainMenu% + 1 '                                                increment main menu entry counter
    AltKey% = INSTR(ReadData$, "&")
    IF AltKey% THEN
        Menu(MainMenu%, 0).altkey = RIGHTALTKEY + ASC(UCASE$(ReadData$), AltKey% + 1)
        Menu(MainMenu%, 0).altkeycharacter = ASC(UCASE$(ReadData$), AltKey% + 1)
        ReadData$ = LEFT$(ReadData$, AltKey% - 1) + RIGHT$(ReadData$, LEN(ReadData$) - AltKey%)
        Menu(MainMenu%, 0).altkeyx = _PRINTWIDTH(LEFT$(ReadData$, AltKey% - 1))
        Menu(MainMenu%, 0).altkeywidth = _PRINTWIDTH(MID$(ReadData$, AltKey%, 1)) - 2
    END IF
    Menu(MainMenu%, 0).text = ReadData$ '                                      yes, save the main menu entry
    Menu(MainMenu%, 0).idnum = MainMenu% * 100 '                               save the main menu entry id number (hundreds place)
    Menu(MainMenu%, 0).width = _PRINTWIDTH(ReadData$) + Mset.spacing * 2 '                             width of main menu entry
    Mset.width = Mset.width + Menu(MainMenu%, 0).width '                                           main menu total width
    Menu(MainMenu%, 0).x = x% '                                                                    left location of main entry box
    Menu(MainMenu%, 0).active = _NEWIMAGE(Menu(MainMenu%, 0).width, Mset.height, 32) '             active main menu entry image (normal)
    _DEST Menu(MainMenu%, 0).active
    _FONT Mset.font
    _PRINTMODE _KEEPBACKGROUND
    Menu(MainMenu%, 0).highlight = _NEWIMAGE(Menu(MainMenu%, 0).width, Mset.height, 32) '          highlighted main menu entry image (mouse over)
    _DEST Menu(MainMenu%, 0).highlight
    _FONT Mset.font
    _PRINTMODE _KEEPBACKGROUND
    Menu(MainMenu%, 0).selected = _NEWIMAGE(Menu(MainMenu%, 0).width, Mset.height, 32) '           selected main menu entry image (left clicked)
    _DEST Menu(MainMenu%, 0).selected
    _FONT Mset.font
    _PRINTMODE _KEEPBACKGROUND
    Menu(MainMenu%, 0).submenu = _NEWIMAGE(MaxLength%(MainMenu%) + 4, (TotalSubmenu%(MainMenu%)) * Mset.height + 4, 32)
    _DEST Menu(MainMenu%, 0).submenu
    _FONT Mset.font
    _PRINTMODE _KEEPBACKGROUND
    Menu(MainMenu%, 0).undersubmenu = _NEWIMAGE(MaxLength%(MainMenu%) + 4 + Mset.shadow, (TotalSubmenu%(MainMenu%)) * Mset.height + 4 + Mset.shadow, 32)
    _DEST Menu(MainMenu%, 0).undersubmenu
    _FONT Mset.font
    _PRINTMODE _KEEPBACKGROUND
    SubMenu% = 0 '                                                                                 reset sub menu entry counter
    READ ReadData$ '                                                                               second entry must be sub menu entry
    DO WHILE ReadData$ <> "*" '                                                                    stop looping when encountered
        SubMenu% = SubMenu% + 1 '                                                                  increment sub menu entry counter
        Menu(MainMenu%, SubMenu%).text = ReadData$ '                                               yes, save the sub menu entry
        Menu(MainMenu%, SubMenu%).idnum = SubMenu% '                                               save the sub menu entry id number (1-99)
        Menu(MainMenu%, SubMenu%).width = MaxLength%(MainMenu%) '                                  width of submenu entry
        Menu(MainMenu%, SubMenu%).x = 2 '                                                          x location of submenu entry
        Menu(MainMenu%, SubMenu%).y = ((SubMenu% - 1) * Mset.height) + 2 '                         y location of submenu entry
        Menu(MainMenu%, SubMenu%).active = _NEWIMAGE(MaxLength%(MainMenu%), Mset.height, 32) '     active (normal) submenu entry image
        _DEST Menu(MainMenu%, SubMenu%).active
        _FONT Mset.font
        _PRINTMODE _KEEPBACKGROUND
        Menu(MainMenu%, SubMenu%).highlight = _NEWIMAGE(MaxLength%(MainMenu%), Mset.height, 32) '  highlighted submenu entry image
        _DEST Menu(MainMenu%, SubMenu%).highlight
        _FONT Mset.font
        _PRINTMODE _KEEPBACKGROUND
        Menu(MainMenu%, SubMenu%).inactive = _NEWIMAGE(MaxLength%(MainMenu%), Mset.height, 32) '   inactive (disabled) submenu entry image
        _DEST Menu(MainMenu%, SubMenu%).inactive
        _FONT Mset.font
        _PRINTMODE _KEEPBACKGROUND
        Menu(MainMenu%, SubMenu%).ihighlight = _NEWIMAGE(MaxLength%(MainMenu%), Mset.height, 32) ' inactive (disabled) highlighted submenu entry image
        _DEST Menu(MainMenu%, SubMenu%).ihighlight
        _FONT Mset.font
        _PRINTMODE _KEEPBACKGROUND
        Menu(MainMenu%, SubMenu%).live = -1 '                                                      submenu entry enabled by default
        IF INSTR(ReadData$, "-") THEN '                                                            should this entry have a line above?
            IF SubMenu% > 1 THEN Menu(MainMenu%, SubMenu%).drawline = -1 '                        yes, remember this
            ReadData$ = RIGHT$(ReadData$, LEN(ReadData$) - 1) '                                        remove the dash
        END IF
        AltKey% = INSTR(ReadData$, "&") '                                                             find an alt key marker
        IF AltKey% THEN '                                                                             found?
            Menu(MainMenu%, SubMenu%).altkey = RIGHTALTKEY + ASC(UCASE$(ReadData$), AltKey% + 1) '    yes, save the alt key combo value
            Menu(MainMenu%, SubMenu%).altkeycharacter = ASC(UCASE$(ReadData$), AltKey% + 1) '         save the actual alt key
            ReadData$ = LEFT$(ReadData$, AltKey% - 1) + RIGHT$(ReadData$, LEN(ReadData$) - AltKey%) ' remove the alt key marker
            Menu(MainMenu%, SubMenu%).altkeyx = _PRINTWIDTH(LEFT$(ReadData$, AltKey% - 1)) '          remember position of alt underline
            Menu(MainMenu%, SubMenu%).altkeywidth = _PRINTWIDTH(MID$(ReadData$, AltKey%, 1)) - 2 '    remember how wide the underline is
        END IF
        IF INSTR(ReadData$, "#") THEN '                                                                      is there a hot key marker?
            Menu(MainMenu%, SubMenu%).ljustify = LEFT$(ReadData$, INSTR(ReadData$, "#") - 1) '               yes, get left submenu text
            Menu(MainMenu%, SubMenu%).rjustify = RIGHT$(ReadData$, LEN(ReadData$) - INSTR(ReadData$, "#")) ' get hotkey right justified text
            '******************************************
            '* Attempt to discover hotkey combination *
            '******************************************
            Rjustify$ = UCASE$(RTRIM$(Menu(MainMenu%, SubMenu%).rjustify))
            WHILE INSTR(Rjustify$, "+") OR INSTR(Rjustify$, " ")
                Plus% = INSTR(Rjustify$, "+")
                IF Plus% THEN Rjustify$ = LEFT$(Rjustify$, Plus% - 1) + RIGHT$(Rjustify$, LEN(Rjustify$) - Plus%)
                Space% = INSTR(Rjustify$, " ")
                IF Space% THEN Rjustify$ = LEFT$(Rjustify$, Space% - 1) + RIGHT$(Rjustify$, LEN(Rjustify$) - Space%)
            WEND
            IF INSTR(Rjustify$, "CTRL") THEN
                Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 100305 ' ** Right CTRL Key
                Rjustify$ = LEFT$(Rjustify$, INSTR(Rjustify$, "CTRL") - 1) + RIGHT$(Rjustify$, LEN(Rjustify$) - (INSTR(Rjustify$, "CTRL") + 3))
            END IF
            IF INSTR(Rjustify$, "SHIFT") THEN
                Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 100303 ' ** Right SHIFT key
                Rjustify$ = LEFT$(Rjustify$, INSTR(Rjustify$, "SHIFT") - 1) + RIGHT$(Rjustify$, LEN(Rjustify$) - (INSTR(Rjustify$, "SHIFT") + 4))
            END IF
            SELECT CASE Rjustify$
                CASE "F1"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 15104 ' ** F1 Key
                CASE "F2"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 15360 ' ** F2 Key
                CASE "F3"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 15616 ' ** F3 Key
                CASE "F4"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 15872 ' ** F4 Key
                CASE "F5"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 16128 ' ** F5 Key
                CASE "F6"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 16384 ' ** F6 Key
                CASE "F7"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 16640 ' ** F7 Key
                CASE "F8"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 16896 ' ** F8 Key
                CASE "F9"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 17152 ' ** F9 Key
                CASE "F10"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 17408 ' ** F10 Key
                CASE "F11"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 34048 ' ** F11 Key
                CASE "F12"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 34304 ' ** F12 Key
                CASE "DEL", "DELETE"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 21248 ' ** DELETE Key
                CASE "INS", "INSERT"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 20992 ' ** INSERT Key
                CASE "HOME"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 18176 ' ** HOME Key
                CASE "END"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 20224 ' ** END Key
                CASE "PGUP", "PAGEUP"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 18688 ' ** PGUP Key
                CASE "PGDN", "PAGEDOWN"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 20736 ' ** PGDN Key
                CASE "RIGHT", "RIGHTARROW"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 19712 ' ** Right Arrow Key (use?)
                CASE "LEFT", "LEFTARROW"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 19200 ' ** Left Arrow Key  (use?)
                CASE "UP", "UPARROW"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 18432 ' ** Up Arrow Key    (use?)
                CASE "DOWN", "DOWNARROW"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 20480 ' ** Down Arrow Key  (use?)
                CASE "CLOCK", "CAPS", "CAPSLOCK"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 100301 ' ** CAPS Lock Key
                CASE "NLOCK", "NUM", "NUMLOCK"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 100300 ' ** NUM Lock Key
                CASE "SLOCK", "SCROLL", "SCROLLLOCK"
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + 100302 ' ** SCROLL Lock Key
                CASE ELSE
                    Menu(MainMenu%, SubMenu%).hotkey = Menu(MainMenu%, SubMenu%).hotkey + ASC(Rjustify$) ' ** any other key
            END SELECT
        ELSE
            Menu(MainMenu%, SubMenu%).ljustify = ReadData$ '                   left text
            Menu(MainMenu%, SubMenu%).rjustify = "" '                          no hotkey
        END IF
        READ ReadData$ '                                                       read next sub menu entry
    LOOP
    Menu(MainMenu%, 0).idnum = Menu(MainMenu%, 0).idnum + SubMenu% '           attach the number of sub menu entries to main id number
    x% = x% + Menu(MainMenu%, 0).width '                                       move to next main menu position
    READ ReadData$ '                                                           read next main menu entry
LOOP
Mset.menubar = _NEWIMAGE(ScreenWidth%, Mset.height, 32) '                          create normal menu bar image
_DEST Mset.menubar '                                                               set it as the destination image
_FONT Mset.font '                                                              set its font
_PRINTMODE _KEEPBACKGROUND '                                                   set its font behavior
Mset.menubarhighlight = _NEWIMAGE(ScreenWidth%, Mset.height, 32) '                 create higlighted menu bar image
_DEST Mset.menubarhighlight '                                                      set it as the destination image
_FONT Mset.font '                                                              set its font
_PRINTMODE _KEEPBACKGROUND '                                                   set its font behavior
Mset.menubarselected = _NEWIMAGE(ScreenWidth%, Mset.height, 32) '                  create selected menu bar image
_DEST Mset.menubarselected '                                                       set it as the destination image
_FONT Mset.font '                                                              set its font
_PRINTMODE _KEEPBACKGROUND '                                                   set its font behavior
Mset.undermenu = _NEWIMAGE(ScreenWidth%, Mset.height, 32)

'******************************************************************************
'* Draw all the menu graphics                                                 *
'******************************************************************************
DrawMenuBars
FOR MainMenu% = 1 TO UBOUND(Menu)
    DrawMainEntry MainMenu%
    DrawSubMenu MainMenu%
    FOR SubMenu% = 1 TO Menu(MainMenu%, 0).idnum - ((Menu(MainMenu%, 0).idnum \ 100) * 100)
        DrawSubEntry MainMenu%, SubMenu%
    NEXT SubMenu%
NEXT MainMenu%
'******************************************************************************
'* routine cleanup                                                            *
'******************************************************************************
REDIM MaxLength%(0) '                                                          array no longer needed, free memory
REDIM TotalSubmenu%(0) '                                                       array no longer needed, free memory
_FREEIMAGE Mset.menubarhighlight '                                                 image no longer needed, free memory
_FREEIMAGE Mset.menubarselected '                                                  image no longer needed, free memory
_DEST DestHold& '                                                              restore original destination when routine called
_FONT FontHold& '                                                              restore original font when routine called

END SUB

'--------------------------------------------------------------------------------------------------------------------------------

