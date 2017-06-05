/* This file is generated - please edit src/config.h instead */
/* Use // to disable options                                                  */

#define    BACKGROUND_IMAGE       ".config/ivte/background.png"
#define BACKGROUND_SATURATION  0.2
#define BACKGROUND_TINT_COLOR  "black"
#define BACKGROUND_TRANSPARENT TRUE  /* Pseudo transparent background      */
                         /*          DELETE_SEQUENCE                          */
                         /*          DELETE_SEQUENCE                          */
#define BELL_AUDIBLE           TRUE
                         /*          ZENBURN_DARK, SOLARIZED_DARK,            */
                         /*          SOLARIZED_LIGHT                          */
#define COMMAND_AT_ROOT_WINDOW TRUE  /* -r option, run in root window   */
#define COMMAND_COLOR_FG       TRUE  /* -fg option, foreground color    */
#define COMMAND_COLOR_BG       TRUE  /* -bg option, background color    */
#define COMMAND_SATURATION     TRUE  /* -sa option, saturation level    */
#define COMMAND_DOCK_MODE      TRUE  /* -d option, run as a dock        */
#define COMMAND_EXEC_PROGRAM   TRUE  /* -e option, execute program      */
#define COMMAND_FULLSCREEN     TRUE  /* -f option, fullscreen mode */
#define COMMAND_FONT           TRUE  /* -fn option, assigns font & size  */
#define COMMAND_GEOMETRY       TRUE  /* -g +X+Y option, assign geometry */
#define COMMAND_LOGIN_SHELL    TRUE  /* -ls option, enable login shell  */
#define COMMAND_SET_TITLE      TRUE  /* -T or -title set program title  */
#define COMMAND_SHOW_HELP      TRUE  /* -h option, show help            */
#define COMMAND_SHOW_OPTIONS   TRUE  /* -o option, show build   options */
#define COMMAND_SHOW_VERSION   TRUE  /* -v option, show program version */
#define CURSOR_BLINKS          TRUE
#define FONT                   "Monospace 9"
#define PROGRAM_ICON           ".config/ivte/icon.png"
#define SCROLL_LINES           2000  /* Negative value means unlimited   */
#define SCROLL_ON_KEYSTROKE    TRUE
#define SCROLL_ON_OUTPUT       TRUE
#define SCROLLBAR              RIGHT /* LEFT, RIGHT, OFF_L, OFF_R */
#define SHOW_WINDOW_BORDER     TRUE
#define SHOW_WINDOW_DECORATED  TRUE
#define SHOW_WINDOW_ICON       TRUE
#define STATUS_BAR             TRUE
#define    WORD_CHARS             "-A-Za-z0-9_$.+!*(),;:@&=?/~#%"
#define    MENU                   TRUE
#define MENU_CUSTOM  "Copy", "Paste", "Select all", "Separator", "Zoom in", "Zoom out", "Zoom default", "Separator", "Add tab", "Remove tab", "New window", "Separator","Toggle tabbar","Toggle scrollbar","Toggle status bar","Separator","Reset and clear","Reset terminal","Separator","Quit"

#define TAB                    TRUE
#define TAB_BORDER             1
#define TAB_CLOSE_BUTTON       TRUE
#define TAB_EXPANDED_WIDTH     TRUE
#define TAB_LABEL              "Terminal %u" /* %u replaced by number */ 
#define TAB_LABEL_DYNAMIC      TRUE      /* for xterm escape sequences */
#define TAB_NEW_PATH_EQUAL_OLD TRUE
#define TAB_REORDERABLE        TRUE
#define TAB_SHOW_INFO_AT_TITLE TRUE
#define TABBAR                 TRUE
#define TABBAR_SCROLLABLE      TRUE
#define TABBAR_MENU_SELECT_TAB TRUE

/* Use || for multiple hotkeys.
 *
 * CTRL(GDK_A) || CTRL(GDK_a) || ALT(GDK_b) || CTRL_ALT(GDK_c) || SHIFT(GDK_d)
 *
 * CTRL(GDK_a)     = "Ctrl + a"
 * CTRL(GDK_A)     = "Ctrl + Shift + a"
 * ALT(GDK_A)      = "Alt  + Shift + a"
 * CTRL_ALT(GDK_A) = "Ctrl + Alt + Shift + a"
 *
 * Also support: ALT_SHIFT(GDK_*)
 *               CTRL_ALT_SHIFT(GDK_*)
 *               CTRL_SHIFT(GDK_*)
 *
 * See /usr/include/gtk-?.0/gdk/gdkkeysyms.h for GDK_* definitions.           */

#define    HOTKEY                       TRUE
#define HOTKEY_SELECT_ALL            CTRL(GDK_A) || CTRL(GDK_a)
#define HOTKEY_PASTE            CTRL(GDK_Y) || CTRL(GDK_y)
#define HOTKEY_COPY                  CTRL(GDK_W) || CTRL(GDK_w) || ALT(GDK_W) || ALT(GDK_w)
#define HOTKEY_TAB_ADD               CTRL(GDK_T) || CTRL(GDK_t)
#define HOTKEY_MIMIC_SCROLL_UP       CTRL(GDK_Up)
#define HOTKEY_MIMIC_SCROLL_DOWN     CTRL(GDK_Down) 
#define HOTKEY_SCROLL_ONE_PAGE_UP    CTRL_SHIFT(GDK_Up)
#define HOTKEY_SCROLL_ONE_PAGE_DOWN  CTRL_SHIFT(GDK_Down) 
#define HOTKEY_SEARCH_STRING         CTRL(GDK_Q) || CTRL(GDK_q)
#define HOTKEY_SEARCH_NEXT           CTRL(GDK_g) || CTRL(GDK_g)

#define CTRL_NUMBER_GO_TO_TAB_NUMBER TRUE
#define HOTKEY_TAB_EDIT_LABEL        CTRL(GDK_Right) || CTRL_SHIFT(GDK_E) || CTRL_SHIFT(GDK_e) || ALT(GDK_M) || ALT (GDK_m)

