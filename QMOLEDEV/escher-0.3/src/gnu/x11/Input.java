package gnu.x11;

/** X keyboard and pointer. */
public class Input {
  // KEYBUTMASK - keyboard button mask
  public static final int SHIFT_MASK = 1<<0;
  public static final int LOCK_MASK = 1<<1; // cap lock
  public static final int CONTROL_MASK = 1<<2;
  public static final int MOD1_MASK = 1<<3; // alt key
  public static final int MOD2_MASK = 1<<4; // num lock
  public static final int MOD3_MASK = 1<<5; // menu key
  public static final int MOD4_MASK = 1<<6; // window key
  public static final int MOD5_MASK = 1<<7; // scroll lock
  public static final int BUTTON1_MASK = 1<<8;
  public static final int BUTTON2_MASK = 1<<9;
  public static final int BUTTON3_MASK = 1<<10;
  public static final int BUTTON4_MASK = 1<<11;
  public static final int BUTTON5_MASK = 1<<12;


  // 104 PC keyboard
  public static final int META_MASK = MOD1_MASK;
  public static final int ALT_MASK = MOD3_MASK;
  public static final int SUPER_MASK = MOD4_MASK;


  public static final int BUTTON1 = 1;
  public static final int BUTTON2 = 2;
  public static final int BUTTON3 = 3;
  public static final int BUTTON4 = 4;
  public static final int BUTTON5 = 5;


  public static final int [] LOCK_COMBINATIONS = {
    0, LOCK_MASK, LOCK_MASK|MOD2_MASK, LOCK_MASK|MOD5_MASK, 
    LOCK_MASK|MOD2_MASK|MOD5_MASK, MOD2_MASK, MOD2_MASK|MOD5_MASK,
    MOD5_MASK
  };


  public Display display;
  public int min_keycode, max_keycode, keysyms_per_keycode;
  public int [] keysyms;
  

  public Input (Display display, int min_keycode, int max_keycode) {
    this.display = display;
    this.min_keycode = min_keycode;
    this.max_keycode = max_keycode;
  }


  // opcode 27 - ungrab pointer
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XUngrabPointer.html">XUngrabPointer</a>
   */
  public void ungrab_pointer (int time) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (27, 0, 2);
      o.write_int32 (time);
      o.send ();
    }
  }


  // opcode 30 - change active pointer grab
  /**
   * @param cursor possible: {@link Cursor#NONE}
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XChangeActivePointerGrab.html">
   *  XChangeActivePointerGrab</a>
   */
  public void change_active_pointer_grab (int event_mask, 
                                          Cursor cursor, int time) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (30, 0, 4);
      o.write_int32 (cursor.id);
      o.write_int32 (time);
      o.write_int16 (event_mask);
      o.send ();
    }
  }


  // opcode 32 - ungrab keyboard
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XUngrabKeyboard.html">XUngrabKeyboard</a>
   */
  public void ungrab_keyboard (int time) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (32, 0, 2);
      o.write_int32 (time);
      o.send ();
    }
  }


  public static final int ASYNC_POINTER = 0;
  public static final int SYNC_POINTER = 1;
  public static final int REPLY_POINTER = 2;
  public static final int ASYNC_KEYBOARD = 3;
  public static final int SYNC_KEYBOARD = 4;
  public static final int REPLY_KEYBOARD = 5;
  public static final int ASYNC_BOTH = 6;
  public static final int SYNC_BOTH = 7;


  // opcode 35 - allow events
  /**
   * @param mode valid:
   * {@link #ASYNC_POINTER},
   * {@link #SYNC_POINTER},
   * {@link #REPLY_POINTER},
   * {@link #ASYNC_KEYBOARD},
   * {@link #SYNC_KEYBOARD},
   * {@link #REPLY_KEYBOARD},
   * {@link #ASYNC_BOTH},
   * {@link #SYNC_BOTH}
   *
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XAllowEvents.html">XAllowEvents</a>
   */
  public void allow_events (int mode, int time) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (35, mode, 2);
      o.write_int32 (time);
      o.send ();
    }
  }


  /** Reply of {@link #input_focus()} */
  public class InputFocusInfo {

    public int revert_to;
    public int focus_id;
    public Window focus;

    InputFocusInfo (ResponseInputStream i) {
      revert_to = i.read_int8 ();
      i.skip (6);
      focus_id = i.read_int32 ();
      if (focus_id != 0 && focus_id != 1)
        focus = (Window) Window.intern (display, focus_id);
    }
  }
  
  
  // opcode 43 - get input focus
  /**
   * @see <a href="XGetInputFocus.html">XGetInputFocus</a>
   */
  public InputFocusInfo input_focus () {

    InputFocusInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (43, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new InputFocusInfo (i);
        i.skip (20);
      }
    }
    return info;
  }


  // opcode 44 - query keymap
  /**
   * @return valid: {@link Enum#next1()}
   * @see <a href="XQueryKeymap.html">XQueryKeymap</a>
   */
  public byte [] query_keymap () {
    RequestOutputStream o = display.out;
    byte [] data = new byte [32];
    synchronized (o) {
      o.begin_request (44, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        i.read_data (data);
      }
    }
    return data;
  }


  // opcode 100 - change keyboard mapping  
  /**
   * @see <a href="XChangeKeyboardMapping.html">XChangeKeyboardMapping</a>
   */
  public void change_keyboard_mapping (int first_keycode, 
                                       int keysyms_per_keycode, int [] keysyms) {
    
    int keycode_count = keysyms.length / keysyms_per_keycode;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (100, keycode_count, 2 + keysyms.length);
      o.write_int8 (first_keycode);
      o.write_int8 (keysyms_per_keycode);
      o.skip (2);

      for (int i = 0; i < keysyms.length; i++)
        o.write_int8 (keysyms [i]);

      o.send ();
    }
  }


  // opcode 101 - get keyboard mapping  
  /**
   * @see <a href="XGetKeyboardMapping.html">XGetKeyboardMapping</a>
   */
  public void keyboard_mapping () {

    int keysym_count = max_keycode - min_keycode + 1;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (101, 0, 2);
      o.write_int8 (min_keycode);
      o.write_int8 (keysym_count);
      o.write_int16 (0); // Unused.

      ResponseInputStream in = display.in;
      synchronized (in) {
        in.read_reply (o);
        in.skip (1);
        keysyms_per_keycode = in.read_int8 ();
        in.skip (2); // Unused.
        int nm = in.read_int32 (); // length.
        assert nm == keysyms_per_keycode * keysym_count;
        in.skip (24); // Unused.
        keysyms = new int [nm];

        for (int i = 0; i < nm; i++) {
          keysyms [i] = in.read_int32 ();
        }
      }
    }
  }


  /** Reply of {@link #keyboard_control()} */
  public class KeyboardControlInfo {

    public boolean global_auto_repeat;
    
    public int led_mask;
    public int key_click_percent;
    public int bell_percent;
    public int bell_pitch;
    public int bell_duration;
    public byte[] auto_repeats;

    KeyboardControlInfo (ResponseInputStream i) {
      global_auto_repeat = i.read_bool ();
      i.skip (6);
      led_mask = i.read_int32 ();
      key_click_percent = i.read_int8 ();
      bell_percent = i.read_int8 ();
      bell_pitch = i.read_int16 ();
      bell_duration = i.read_int16 ();
      i.skip (2);
      auto_repeats = new byte [32];
      i.read_data (auto_repeats);
    }
  }
  
  
  /** X keyboard control. */
  public static class KeyboardControl extends ValueList { // TODO
    public KeyboardControl () { super (8); }
  
  
    public static final int OFF = 0;
    public static final int ON = 1;
  
  
    public static final String [] GLOBAL_AUTO_REPEAT_STRINGS
      = {"off", "on"};
  }


  // opcode 102 - change keyboard control
  /**
   * @see <a href="XChangeKeyboardControl.html">XChangeKeyboardControl</a>
   */
  public void change_keyboard_control (KeyboardControl control) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (102, 0, 2 + control.count ());
      o.write_int32 (control.bitmask);
      control.write (o);
      o.send ();
    }
  }


  // opcode 103 - get keyboard control
  /**
   * @see <a href="XGetKeyboardControl.html">XGetKeyboardControl</a>
   */
  public KeyboardControlInfo keyboard_control () {
    KeyboardControlInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (103, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new KeyboardControlInfo (i);
      }
    }
    return info;
  }


  // opcode 105 - change pointer control
  /**
   * @see <a href="XChangePointerControl.html">XChangePointerControl</a>
   */
  public void change_pointer_control (boolean do_accel, boolean do_threshold,
                                      int accel_numerator,
                                      int accel_denominator, int threshold) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (105, 0, 3);
      o.write_int16 (accel_numerator);
      o.write_int16 (accel_denominator);
      o.write_int16 (threshold);
      o.write_bool (do_accel);
      o.write_bool (do_threshold);
      o.send ();
    }
  }


  /** Reply of {@link #pointer_control()}. */
  public class PointerControlInfo {

    public int acceleration_numerator;
    public int acceleration_denominator;
    public int treshold;

    PointerControlInfo (ResponseInputStream i) {
      acceleration_numerator = i.read_int16 ();
      acceleration_denominator = i.read_int16 ();
      treshold = i.read_int16 ();
    }
  }
  
  
  // opcode 106 - get pointer control
  /**
   * @see <a href="XGetPointerControl.html">XGetPointerControl</a>
   */
  public PointerControlInfo pointer_control () {
    PointerControlInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (106, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        info = new PointerControlInfo (i);
        i.skip(18);
      }
    }
    return info;
  }


  public static final int SUCCESS = 0;
  public static final int BUSY = 1;


  // opcode 116 - set pointer mapping
  /**
   * @return valid:
   * {@link #SUCCESS},
   * {@link #BUSY}
   *
   * @see <a href="XSetPointerMapping.html">XSetPointerMapping</a>
   */
  public int set_pointer_mapping (byte [] map) {

    int n = map.length;
    int p = RequestOutputStream.pad (n);

    int status;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (116, map.length, 1 + (n + p) / 4);
      o.write_bytes (map);
      o.skip (p);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        status = i.read_int8 ();
        i.skip (30);
      }
    }
    return status;
  }


  // opcode 117 - get pointer mapping
  /**
   * @see <a href="XGetPointerMapping.html">XGetPointerMapping</a>
   */
  public byte [] get_pointer_mapping () {

    byte [] map;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (117, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        int len = i.read_int8 ();
        i.skip (30);
        map = new byte [len];
        i.read_data (map);
      }
    }
    return map;
  }


  public static final int FAILED = 2;


  // opcode 118 - set modifier mapping
  /**
   * @return valid:
   * {@link #SUCCESS},
   * {@link #BUSY},
   * {@link #FAILED}
   *
   * @see <a href="XSetModifierMapping.html">XSetModifierMapping</a>
   */
  public int set_modifier_mapping (int keycodes_per_modifier, 
                                   byte [] keycodes) {

    int status;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (118, keycodes_per_modifier,
                       1 + 2 * keycodes_per_modifier);
      o.write (keycodes);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        status = i.read_int8 ();
        i.skip (30);
      }
    }
    return status;
  }

  public class ModifierMapping {

    public int keycodes_per_modifier;
    byte [] map;

    ModifierMapping (ResponseInputStream i) {
      keycodes_per_modifier = i.read_int8 ();
      i.skip (30);
      map = new byte [keycodes_per_modifier * 8];
      i.read_data (map);
    }
  }

  // opcode 119 - get modifier mapping
  /**
   * @return valid: {@link Enum#next1()}
   * @see <a href="XModifierKeymap.html">XModifierKeymap</a>
   */
  public ModifierMapping modifier_mapping () {

    ModifierMapping map;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (119, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        map = new ModifierMapping (i);        
      }
    }
    return map;
  }


  public static final String [] KEYBUT_STRINGS = {
    "shift", "lock", "control", "mod1", "mod2", "mod3", "mod4", "mod5",
    "button1", "button2", "button3", "button4", "button5"
  };


  public static void dump_keybut_mask (int m) {
    for (int i=0; i<KEYBUT_STRINGS.length; i++)
      if (((m & 0x1fff) & 1 << i) != 0)
	System.out.print (KEYBUT_STRINGS [i] + " ");
  }

  /**
   * Maps a keycode to a keysym.
   *
   * @param keycode the keycode
   * @param keystate the modifiers
   *
   * @return the keysym for the specified key code and modifier mask
   */
  public int keycode_to_keysym (int keycode, int keystate) {
    return keycode_to_keysym(keycode, keystate, false);
  }

  /**
   * Maps a keycode to a keysym. When <code>ignore_modifiers</code> is
   * <code>true</code> then this returns the plain keysymbol, independent
   * of the modifiers. Otherwise it returns the real symbol.
   *
   * @param keycode the keycode
   * @param keystate the modifiers
   * @param ignore_modifiers <code>true</code> for returning plain
   *        keysyms, <code>false</code> for taking the modifiers
   *        into account
   *
   * @return the keysym for the specified key code and modifier mask
   */
  public int keycode_to_keysym (int keycode, int keystate,
                                boolean ignore_modifiers) {
    if (keycode > max_keycode) 
      throw new java.lang.Error ("Invalid keycode: " + keycode);

    int keysym = 0;
    int keysym_no = 0;

    // TODO: Maybe add handling of other modifiers.
    if (! ignore_modifiers) {
      if ((keystate & SHIFT_MASK) != 0)
        keysym_no = 1;
      else if ((keystate & MOD5_MASK) != 0) // Alt Gr
        keysym_no = 2; // TODO: 4 seems also valid.
    }

    int index = (keycode - min_keycode) * keysyms_per_keycode + keysym_no;
    keysym = keysyms[index];
    return keysym;
  }


  public int keysym_to_keycode (int keysym) {
    // linear lookup - expensive?
    //for (int i=0; i<keysyms.length; i++)
    // FIXME, hacked to do it in reverse order for solaris

    for (int i=keysyms.length-1; i>=0; i--)
      if (keysyms [i] == keysym)
	return i + min_keycode;
    
    throw new java.lang.Error ("Invalid keysym: " + keysym);	
  }


  /** 
   * Input#ungrab_keyboard(int)
   */
  public void ungrab_keyboard () {
    ungrab_keyboard (Display.CURRENT_TIME);
  }
}
