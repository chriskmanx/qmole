package gnu.x11.keysym;


/**
 * XFree86-specific keyboard symbols. Imported from
 * <code>/usr/include/X11/XF86keysym.h</code>.
 */
public class XFree86 {
  /* ModeLock. This one is old, and not really used any more since XKB
   * offers this functionality.
   */

  public static final int MODE_LOCK = 0x1008ff01; /* mode switch lock */

  
  /* "Internet" keyboards. */

  public static final int STANDBY = 0x1008ff10;
  public static final int AUDIO_LOWER_VOLUME = 0x1008ff11;
  public static final int AUDIO_MUTE = 0x1008ff12;
  public static final int AUDIO_RAISE_VOLUME = 0x1008ff13;
  public static final int AUDIO_PLAY = 0x1008ff14;
  public static final int AUDIO_STOP = 0x1008ff15;
  public static final int AUDIO_PREV = 0x1008ff16;
  public static final int AUDIO_NEXT = 0x1008ff17;
  public static final int HOME_PAGE = 0x1008ff18;
  public static final int MAIL = 0x1008ff19;
  public static final int START = 0x1008ff1a;
  public static final int SEARCH = 0x1008ff1b;
  public static final int AUDIO_RECORD = 0x1008ff1c;


  /* PDA's (e.g. Palm, PocketPC or elsewhere). */

  public static final int CALCULATOR = 0x1008ff1d;
  public static final int MEMO = 0x1008ff1e;
  public static final int TO_DO_LIST = 0x1008ff1f;
  public static final int CALENDAR = 0x1008ff20;
  public static final int POWER_DOWN = 0x1008ff21;
  public static final int CONTRASTADJUST = 0x1008ff22;
  public static final int ROCKER_UP = 0x1008ff23;
  public static final int ROCKER_DOWN = 0x1008ff24;
  public static final int ROCKER_ENTER = 0x1008ff25;
  public static final int BACK = 0x1008ff26;
  public static final int FORWARD = 0x1008ff27;
  public static final int STOP = 0x1008ff28;
  public static final int REFRESH = 0x1008ff29;
  public static final int POWER_OFF = 0x1008ff1a;
  public static final int WAKE_UP = 0x1008ff1b;


  /* Note, 0x1008ff02 - 0x1008ff0f are free and should be used for misc new
   * keysyms that don't fit into any of the groups below.
   */

  
  /* Misc. */

  public static final int FAVORITES = 0x1008ff30;
  public static final int AUDIO_PAUSE = 0x1008ff31;
  public static final int AUDIO_MEDIA = 0x1008ff32;
  public static final int MY_COMPUTER = 0x1008ff33;
  public static final int VENDOR_HOME = 0x1008ff34;
  public static final int LIGHT_BULB = 0x1008ff35;
  public static final int SHOP = 0x1008ff36;  
}
