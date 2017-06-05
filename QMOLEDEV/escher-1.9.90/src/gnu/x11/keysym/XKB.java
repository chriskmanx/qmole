package gnu.x11.keysym;


/**
 * ISO 9995-specific keyboard symbols. Imported from
 * <code>XK_XKB_KEYS</code> section of
 * <code>/usr/include/X11/keysymdef.h</code>. 
 * Note that byte 3 = 0xfe.
 */
public class XKB {
  public static final int LOCK = 0xfe01;
  public static final int LEVEL2_LATCH = 0xfe02;
  public static final int LEVEL3_SHIFT = 0xfe03;
  public static final int LEVEL3_LATCH = 0xfe04;
  public static final int LEVEL3_LOCK = 0xfe05;
  public static final int GROUP_SHIFT = 0xff7e; /* alias for mode_switch */
  public static final int GROUP_LATCH = 0xfe06;
  public static final int GROUP_LOCK = 0xfe07;
  public static final int NEXT_GROUP = 0xfe08;
  public static final int NEXT_GROUP_LOCK = 0xfe09;
  public static final int PREV_GROUP = 0xfe0a;
  public static final int PREV_GROUP_LOCK = 0xfe0b;
  public static final int FIRST_GROUP = 0xfe0c;
  public static final int FIRST_GROUP_LOCK = 0xfe0d;
  public static final int LAST_GROUP = 0xfe0e;
  public static final int LAST_GROUP_LOCK = 0xfe0f;

  public static final int LEFT_TAB = 0xfe20;;
  public static final int MOVE_LINE_UP = 0xfe21;
  public static final int MOVE_LINE_DOWN = 0xfe22;
  public static final int PARTIAL_LINE_UP = 0xfe23;
  public static final int PARTIAL_LINE_DOWN = 0xfe24;
  public static final int PARTIAL_SPACE_LEFT = 0xfe25;
  public static final int PARTIAL_SPACE_RIGHT = 0xfe26;
  public static final int SET_MARGIN_LEFT = 0xfe27;
  public static final int SET_MARGIN_RIGHT = 0xfe28;
  public static final int RELEASE_MARGIN_LEFT = 0xfe29;
  public static final int RELEASE_MARGIN_RIGHT = 0xfe2a;
  public static final int RELEASE_BOTH_MARGINS = 0xfe2b;
  public static final int FAST_CURSOR_LEFT = 0xfe2c;
  public static final int FAST_CURSOR_RIGHT = 0xfe2d;
  public static final int FAST_CURSOR_UP = 0xfe2e;
  public static final int FAST_CURSOR_DOWN = 0xfe2f;
  public static final int CONTINUOUS_UNDERLINE = 0xfe30;
  public static final int DISCONTINUOUS_UNDERLINE = 0xfe31;
  public static final int EMPHASIZE = 0xfe32;
  public static final int CENTER_OBJECT = 0xfe33;
  public static final int ENTER = 0xfe34;

  public static final int DEAD_GRAVE = 0xfe50;;
  public static final int DEAD_ACUTE = 0xfe51;
  public static final int DEAD_CIRCUMFLEX = 0xfe52;
  public static final int DEAD_TILDE = 0xfe53;
  public static final int DEAD_MACRON = 0xfe54;
  public static final int DEAD_BREVE = 0xfe55;
  public static final int DEAD_ABOVEDOT = 0xfe56;
  public static final int DEAD_DIAERESIS = 0xfe57;
  public static final int DEAD_ABOVERING = 0xfe58;
  public static final int DEAD_DOUBLEACUTE = 0xfe59;
  public static final int DEAD_CARON = 0xfe5a;
  public static final int DEAD_CEDILLA = 0xfe5b;
  public static final int DEAD_OGONEK = 0xfe5c;
  public static final int DEAD_IOTA = 0xfe5d;
  public static final int DEAD_VOICED_SOUND = 0xfe5e;
  public static final int DEAD_SEMIVOICED_SOUND = 0xfe5f;
  public static final int DEAD_BELOWDOT = 0xfe60;
  public static final int DEAD_HOOK = 0xfe61;
  public static final int DEAD_HORN = 0xfe62;

  public static final int FIRST_VIRTUAL_SCREEN = 0xfed0;;
  public static final int PREV_VIRTUAL_SCREEN = 0xfed1;
  public static final int NEXT_VIRTUAL_SCREEN = 0xfed2;
  public static final int LAST_VIRTUAL_SCREEN = 0xfed4;
  public static final int TERMINATE_SERVER = 0xfed5;

  public static final int ACCESS_X_ENABLE = 0xfe70;;
  public static final int ACCESS_X_FEEDBACK_ENABLE = 0xfe71;
  public static final int REPEAT_KEYS_ENABLE = 0xfe72;
  public static final int SLOW_KEYS_ENABLE = 0xfe73;
  public static final int BOUNCE_KEYS_ENABLE = 0xfe74;
  public static final int STICKY_KEYS_ENABLE = 0xfe75;
  public static final int MOUSE_KEYS_ENABLE = 0xfe76;
  public static final int MOUSE_KEYS_ACCEL_ENABLE = 0xfe77;
  public static final int OVERLAY1_ENABLE = 0xfe78;
  public static final int OVERLAY2_ENABLE = 0xfe79;
  public static final int AUDIBLE_BELL_ENABLE = 0xfe7a;

  public static final int POINTER_LEFT = 0xfee0;;
  public static final int POINTER_RIGHT = 0xfee1;
  public static final int POINTER_UP = 0xfee2;
  public static final int POINTER_DOWN = 0xfee3;
  public static final int POINTER_UP_LEFT = 0xfee4;
  public static final int POINTER_UP_RIGHT = 0xfee5;
  public static final int POINTER_DOWN_LEFT = 0xfee6;
  public static final int POINTER_DOWN_RIGHT = 0xfee7;
  public static final int POINTER_BUTTON_DFLT = 0xfee8;
  public static final int POINTER_BUTTON1 = 0xfee9;
  public static final int POINTER_BUTTON2 = 0xfeea;
  public static final int POINTER_BUTTON3 = 0xfeeb;
  public static final int POINTER_BUTTON4 = 0xfeec;
  public static final int POINTER_BUTTON5 = 0xfeed;
  public static final int POINTER_DBL_CLICK_DFLT = 0xfeee;
  public static final int POINTER_DBL_CLICK1 = 0xfeef;
  public static final int POINTER_DBL_CLICK2 = 0xfef0;
  public static final int POINTER_DBL_CLICK3 = 0xfef1;
  public static final int POINTER_DBL_CLICK4 = 0xfef2;
  public static final int POINTER_DBL_CLICK5 = 0xfef3;
  public static final int POINTER_DRAG_DFLT = 0xfef4;
  public static final int POINTER_DRAG1 = 0xfef5;
  public static final int POINTER_DRAG2 = 0xfef6;
  public static final int POINTER_DRAG3 = 0xfef7;
  public static final int POINTER_DRAG4 = 0xfef8;
  public static final int POINTER_DRAG5 = 0xfefd;

  public static final int POINTER_ENABLE_KEYS = 0xfef9;;
  public static final int POINTER_ACCELERATE = 0xfefa;
  public static final int POINTER_DFLT_BTN_NEXT = 0xfefb;
  public static final int POINTER_DFLT_BTN_PREV = 0xfefc;
}
