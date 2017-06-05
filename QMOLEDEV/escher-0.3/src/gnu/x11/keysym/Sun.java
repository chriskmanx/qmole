package gnu.x11.keysym;


/**
 * Sun-specific keyboard symbols. Imported from
 * <code>/usr/include/X11/Sunkeysym.h</code>.
 */
public class Sun {
  /* Floating Accent. */

  public static final int FA_GRAVE = 0x1005ff00;
  public static final int FA_CIRCUM = 0x1005ff01;
  public static final int FA_TILDE = 0x1005ff02;
  public static final int FA_ACUTE = 0x1005ff03;
  public static final int FA_DIAERESIS = 0x1005ff04;
  public static final int FA_CEDILLA = 0x1005ff05;

  
  /* Miscellaneous Functions. */

  public static final int F36 = 0x1005ff10; /* labeled f11 */
  public static final int F37 = 0x1005ff11; /* labeled f12 */

  
  /* International & Multi-Key Character Composition. */

  public static final int SYS_REQ = 0x1005ff60;
  public static final int PRINT_SCREEN = 0x0000ff61; /* same as xk_print */
  public static final int COMPOSE = 0x0000ff20; /* same as xk_multi_key */
  public static final int ALT_GRAPH = 0x0000ff7e; /* same as xk_mode_switch */


  /* Cursor Control. */ 

  public static final int PAGE_UP = 0x0000ff55; /* same as xk_prior */
  public static final int PAGE_DOWN = 0x0000ff56; /* same as xk_next */


  /* Open Look Functions. */

  public static final int UNDO = 0x0000ff65; /* same as xk_undo */
  public static final int AGAIN = 0x0000ff66; /* same as xk_redo */
  public static final int FIND = 0x0000ff68; /* same as xk_find */
  public static final int STOP = 0x0000ff69; /* same as xk_cancel */
  public static final int PROPS = 0x1005ff70;
  public static final int FRONT = 0x1005ff71;
  public static final int COPY = 0x1005ff72;
  public static final int OPEN = 0x1005ff73;
  public static final int PASTE = 0x1005ff74;
  public static final int CUT = 0x1005ff75;
  public static final int POWER_SWITCH = 0x1005ff76;
  public static final int AUDIO_LOWER_VOLUME = 0x1005ff77;
  public static final int AUDIO_MUTE = 0x1005ff78;
  public static final int AUDIO_RAISE_VOLUME = 0x1005ff79;
  public static final int VIDEO_DEGAUSS = 0x1005ff7a;
  public static final int VIDEO_LOWER_BRIGHTNESS = 0x1005ff7b;
  public static final int VIDEO_RAISE_BRIGHTNESS = 0x1005ff7c;
  public static final int POWER_SWITCH_SHIFT = 0x1005ff7d;
}
