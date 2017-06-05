package gnu.x11.keysym;


/**
 * Miscellaneous keyboard symbols. Imported from <code>XK_MISCELLANY</code>
 * section of <code>/usr/include/X11/keysymdef.h</code>.
 */
public class Misc {
  /* I use the following emacs lisp code together with some other (dirty)
   * keyboard macro hack to convert `/usr/include/X11/*keysym*.h' to java
   * defintions.
   *
   * (defun mixed-case-to-lisp-case (string)
   *   "Convert SomeMixedCaseString to a_more_readable_lisp_case_string.
   * E.g. XK_Armenian_ligature_ew _> xk_armenian_ligature_ew."
   *   (downcase
   *    (char-list-to-string 
   *     (loop
   *       with previous = ? 
   *       for current in (string-to-char-list string)
   *       if (and (lowercasep previous) (uppercasep current))
   *       collect ?_ and collect current
   *       else collect current
   *       do (setq previous current)))))   
   */


  public static final int VOID_SYMBOL = 0xffffff;


  /* TTY Functions, cleverly chosen to map to ascii, for convenience of
   * programming, but could have been arbitrary (at the cost of lookup
   * tables in client code).
   */

  public static final int BACKSPACE = 0xff08; /* back space, back char */
  public static final int TAB = 0xff09;
  public static final int LINEFEED = 0xff0a; /* linefeed, LF */
  public static final int CLEAR = 0xff0b;
  public static final int RETURN = 0xff0d; /* return, enter */
  public static final int PAUSE = 0xff13; /* pause, hold */
  public static final int SCROLL_LOCK = 0xff14;
  public static final int SYS_REQ = 0xff15;
  public static final int ESCAPE = 0xff1b;
  public static final int DELETE = 0xffff; /* delete, rubout */


  /* International & multi-key character composition. */

  public static final int MULTI_KEY = 0xff20; /* multi-key character compose */
  public static final int CODEINPUT = 0xff37;
  public static final int SINGLE_CANDIDATE = 0xff3c;
  public static final int MULTIPLE_CANDIDATE = 0xff3d;
  public static final int PREVIOUS_CANDIDATE = 0xff3e;



  /* Japanese keyboard support. 0xff31 thru 0xff3f are under XK_KOREAN. */  
  
  public static final int KANJI = 0xff21; /* kanji, kanji convert */
  public static final int MUHENKAN = 0xff22; /* cancel conversion */
  public static final int HENKAN_MODE = 0xff23; /* start/stop conversion */
  public static final int HENKAN = 0xff23; /* alias for henkan_mode */
  public static final int ROMAJI = 0xff24; /* to romaji */
  public static final int HIRAGANA = 0xff25; /* to hiragana */
  public static final int KATAKANA = 0xff26; /* to katakana */
  public static final int HIRAGANA_KATAKANA = 0xff27; /* hiragana/katakana toggle */
  public static final int ZENKAKU = 0xff28; /* to zenkaku */
  public static final int HANKAKU = 0xff29; /* to hankaku */
  public static final int ZENKAKU_HANKAKU = 0xff2a; /* zenkaku/hankaku toggle */
  public static final int TOUROKU = 0xff2b; /* add to dictionary */
  public static final int MASSYO = 0xff2c; /* delete from dictionary */
  public static final int KANA_LOCK = 0xff2d; /* kana lock */
  public static final int KANA_SHIFT = 0xff2e; /* kana shift */
  public static final int EISU_SHIFT = 0xff2f; /* alphanumeric shift */
  public static final int EISU_TOGGLE = 0xff30; /* alphanumeric toggle */
  public static final int KANJI_BANGOU = 0xff37; /* codeinput */
  public static final int ZEN_KOHO = 0xff3d; /* multiple/all candidate(s) */
  public static final int MAE_KOHO = 0xff3e; /* previous candidate */


  /** Cursor control & motion. */

  public static final int HOME = 0xff50;
  public static final int LEFT = 0xff51; /* move left, left arrow */
  public static final int UP = 0xff52; /* move up, up arrow */
  public static final int RIGHT = 0xff53; /* move right, right arrow */
  public static final int DOWN = 0xff54; /* move down, down arrow */
  public static final int PRIOR = 0xff55; /* prior, previous */
  public static final int PAGE_UP = 0xff55;
  public static final int NEXT = 0xff56; /* next */
  public static final int PAGE_DOWN = 0xff56;
  public static final int END = 0xff57; /* eol */
  public static final int BEGIN = 0xff58; /* bol */


  /* Misc Functions. */

  public static final int SELECT = 0xff60; /* select, mark */
  public static final int PRINT = 0xff61;
  public static final int EXECUTE = 0xff62; /* execute, run, do */
  public static final int INSERT = 0xff63; /* insert, insert here */
  public static final int UNDO = 0xff65; /* undo, oops */
  public static final int REDO = 0xff66; /* redo, again */
  public static final int MENU = 0xff67;
  public static final int FIND = 0xff68; /* find, search */
  public static final int CANCEL = 0xff69; /* cancel, stop, abort, exit */
  public static final int HELP = 0xff6a; /* help */
  public static final int BREAK = 0xff6b;
  public static final int MODE_SWITCH = 0xff7e; /* character set switch */
  public static final int SCRIPT_SWITCH = 0xff7e; /* alias for mode_switch */
  public static final int NUM_LOCK = 0xff7f;


  /* Keypad Functions, keypad numbers cleverly chosen to map to ascii. */

  public static final int KP_SPACE = 0xff80; /* space */
  public static final int KP_TAB = 0xff89;
  public static final int KP_ENTER = 0xff8d; /* enter */
  public static final int KP_F1 = 0xff91; /* pf1, kp_a, ... */
  public static final int KP_F2 = 0xff92;
  public static final int KP_F3 = 0xff93;
  public static final int KP_F4 = 0xff94;
  public static final int KP_HOME = 0xff95;
  public static final int KP_LEFT = 0xff96;
  public static final int KP_UP = 0xff97;
  public static final int KP_RIGHT = 0xff98;
  public static final int KP_DOWN = 0xff99;
  public static final int KP_PRIOR = 0xff9a;
  public static final int KP_PAGE_UP = 0xff9a;
  public static final int KP_NEXT = 0xff9b;
  public static final int KP_PAGE_DOWN = 0xff9b;
  public static final int KP_END = 0xff9c;
  public static final int KP_BEGIN = 0xff9d;
  public static final int KP_INSERT = 0xff9e;
  public static final int KP_DELETE = 0xff9f;
  public static final int KP_EQUAL = 0xffbd; /* equals */
  public static final int KP_MULTIPLY = 0xffaa;
  public static final int KP_ADD = 0xffab;
  public static final int KP_SEPARATOR = 0xffac; /* separator, often comma */
  public static final int KP_SUBTRACT = 0xffad;
  public static final int KP_DECIMAL = 0xffae;
  public static final int KP_DIVIDE = 0xffaf;

  public static final int KP_0 = 0xffb0;;
  public static final int KP_1 = 0xffb1;
  public static final int KP_2 = 0xffb2;
  public static final int KP_3 = 0xffb3;
  public static final int KP_4 = 0xffb4;
  public static final int KP_5 = 0xffb5;
  public static final int KP_6 = 0xffb6;
  public static final int KP_7 = 0xffb7;
  public static final int KP_8 = 0xffb8;
  public static final int KP_9 = 0xffb9;



  /* Auxilliary Functions; note the duplicate definitions for left and
   * right function keys; Sun keyboards and a few other manufactures have
   * such function key groups on the left and/or right sides of the
   * keyboard. We've not found a keyboard with more than 35 function keys
   * total.
   */

  public static final int F1 = 0xffbe;
  public static final int F2 = 0xffbf;
  public static final int F3 = 0xffc0;
  public static final int F4 = 0xffc1;
  public static final int F5 = 0xffc2;
  public static final int F6 = 0xffc3;
  public static final int F7 = 0xffc4;
  public static final int F8 = 0xffc5;
  public static final int F9 = 0xffc6;
  public static final int F10 = 0xffc7;
  public static final int F11 = 0xffc8;
  public static final int L1 = 0xffc8;
  public static final int F12 = 0xffc9;
  public static final int L2 = 0xffc9;
  public static final int F13 = 0xffca;
  public static final int L3 = 0xffca;
  public static final int F14 = 0xffcb;
  public static final int L4 = 0xffcb;
  public static final int F15 = 0xffcc;
  public static final int L5 = 0xffcc;
  public static final int F16 = 0xffcd;
  public static final int L6 = 0xffcd;
  public static final int F17 = 0xffce;
  public static final int L7 = 0xffce;
  public static final int F18 = 0xffcf;
  public static final int L8 = 0xffcf;
  public static final int F19 = 0xffd0;
  public static final int L9 = 0xffd0;
  public static final int F20 = 0xffd1;
  public static final int L10 = 0xffd1;
  public static final int F21 = 0xffd2;
  public static final int R1 = 0xffd2;
  public static final int F22 = 0xffd3;
  public static final int R2 = 0xffd3;
  public static final int F23 = 0xffd4;
  public static final int R3 = 0xffd4;
  public static final int F24 = 0xffd5;
  public static final int R4 = 0xffd5;
  public static final int F25 = 0xffd6;
  public static final int R5 = 0xffd6;
  public static final int F26 = 0xffd7;
  public static final int R6 = 0xffd7;
  public static final int F27 = 0xffd8;
  public static final int R7 = 0xffd8;
  public static final int F28 = 0xffd9;
  public static final int R8 = 0xffd9;
  public static final int F29 = 0xffda;
  public static final int R9 = 0xffda;
  public static final int F30 = 0xffdb;
  public static final int R10 = 0xffdb;
  public static final int F31 = 0xffdc;
  public static final int R11 = 0xffdc;
  public static final int F32 = 0xffdd;
  public static final int R12 = 0xffdd;
  public static final int F33 = 0xffde;
  public static final int R13 = 0xffde;
  public static final int F34 = 0xffdf;
  public static final int R14 = 0xffdf;
  public static final int F35 = 0xffe0;
  public static final int R15 = 0xffe0;


  /* Modifiers. */

  public static final int SHIFT_L = 0xffe1; /* left shift */
  public static final int SHIFT_R = 0xffe2; /* right shift */
  public static final int CONTROL_L = 0xffe3; /* left control */
  public static final int CONTROL_R = 0xffe4; /* right control */
  public static final int CAPS_LOCK = 0xffe5; /* caps lock */
  public static final int SHIFT_LOCK = 0xffe6; /* shift lock */

  public static final int META_L = 0xffe7; /* left meta */
  public static final int META_R = 0xffe8; /* right meta */
  public static final int ALT_L = 0xffe9; /* left alt */
  public static final int ALT_R = 0xffea; /* right alt */
  public static final int SUPER_L = 0xffeb; /* left super */
  public static final int SUPER_R = 0xffec; /* right super */
  public static final int HYPER_L = 0xffed; /* left hyper */
  public static final int HYPER_R = 0xffee; /* right hyper */  
}
