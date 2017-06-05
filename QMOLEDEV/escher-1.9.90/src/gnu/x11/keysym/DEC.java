package gnu.x11.keysym;


/**
 * DEC-specific keyboard symbols. Imported from
 * <code>/usr/include/X11/DECkeysym.h</code>. Note that 29th bit is set.
 */
public class DEC {
  /* Two-key compose sequence initiators, chosen to map to Latin1
   * characters.
   */

  public static final int RING_ACCENT = 0x1000feb0;
  public static final int CIRCUMFLEX_ACCENT = 0x1000fe5e;
  public static final int CEDILLA_ACCENT = 0x1000fe2c;
  public static final int ACUTE_ACCENT = 0x1000fe27;
  public static final int GRAVE_ACCENT = 0x1000fe60;
  public static final int TILDE = 0x1000fe7e;
  public static final int DIAERESIS = 0x1000fe22;


  /* Special keysym for LK2** "Remove" key on editing keypad. */

  public static final int REMOVE = 0x1000ff00;
}
