package gnu.x11;

/** X keyboard and pointer. */
public class Input {
    
  public enum KeyMask {
      // KEYBUTMASK - keyboard button mask
      SHIFT_MASK(1<<0),
      LOCK_MASK(1<<1), // cap lock
      CONTROL_MASK(1<<2),
      MOD1_MASK(1<<3), // alt key
      MOD2_MASK(1<<4), // num lock
      MOD3_MASK(1<<5), // menu key
      MOD4_MASK(1<<6), // window key
      MOD5_MASK(1<<7), // scroll lock
      BUTTON1_MASK(1<<8),
      BUTTON2_MASK(1<<9),
      BUTTON3_MASK(1<<10),
      BUTTON4_MASK(1<<11),
      BUTTON5_MASK(1<<12),
      
      // 104 PC keyboard
      META_MASK(1<<3),
      ALT_MASK(1<<5),
      SUPER_MASK(1<<6),
      
      // Mouse
      BUTTON1(1),
      BUTTON2(2),
      BUTTON3(3),
      BUTTON4(4),
      BUTTON5(5);
      
      private int code;
      
      KeyMask(int cd) {
          this.code =cd;
      }
      
      public int getCode() {
          return code;
      }
      
      public int logicOr(KeyMask km) {
          return this.getCode() | km.getCode();
      }
      
      public int logicOr(int i) {
          return this.getCode() | i;
      }
      
      public static KeyMask getButton(int buttonID) {
          switch (buttonID) {
            case 1:
                return BUTTON1;
            case 2:
                return BUTTON2;
            case 3:
                return BUTTON3;
            default:
                return BUTTON1;
          }
      }
  }

  public static final int [] LOCK_COMBINATIONS = {
    0,
    KeyMask.LOCK_MASK.getCode(),
    KeyMask.LOCK_MASK.logicOr(KeyMask.MOD2_MASK),
    KeyMask.LOCK_MASK.logicOr(KeyMask.MOD5_MASK),
    KeyMask.LOCK_MASK.logicOr(KeyMask.MOD2_MASK.logicOr(KeyMask.MOD5_MASK)),
    KeyMask.MOD2_MASK.getCode(),
    KeyMask.MOD2_MASK.logicOr(KeyMask.MOD5_MASK),
    KeyMask.MOD5_MASK.getCode()
  };


  public enum InputEvent {
      ASYNC_POINTER(0),
      SYNC_POINTER(1),
      REPLY_POINTER(2),
      ASYNC_KEYBOARD(3),
      SYNC_KEYBOARD(4),
      REPLY_KEYBOARD(5),
      ASYNC_BOTH(6),
      SYNC_BOTH(7);
      private int code;
      
      InputEvent(int cd) {
          this.code =cd;
      }
      
      public int getCode() {
          return code;
      }

  }
  
  public enum Status {
      SUCCESS(0),
      BUSY(1),
      FAILED(2);
      
      private int code;
      
      Status(int cd) {
          this.code =cd;
      }
      
      public int getCode() {
          return code;
      }
      
      public static Status getStatus(int code) {
          switch (code)
          {
              case 0: return Status.SUCCESS;
              case 1: return Status.BUSY;
              case 2: return Status.FAILED;
              
              default: return Status.SUCCESS;
          }
      }
  }

  private Display display;
  private int minKeycode, maxKeycode, keysymsPerKeycode;
  private int [] keysyms;
  

  public Input (Display display, int min_keycode, int max_keycode) {
    this.display = display;
    this.minKeycode = min_keycode;
    this.maxKeycode = max_keycode;
  }


  // opcode 27 - ungrab pointer
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XUngrabPointer.html">XUngrabPointer</a>
   */
  public void ungrabPointer (int time) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (27, 0, 2);
      o.writeInt32 (time);
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
  public void changeActivePointerGrab (int event_mask, 
                                          Cursor cursor, int time) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (30, 0, 4);
      o.writeInt32 (cursor.id);
      o.writeInt32 (time);
      o.writeInt16 (event_mask);
      o.send ();
    }
  }


  // opcode 32 - ungrab keyboard
  /**
   * @param time possible: {@link Display#CURRENT_TIME}
   * @see <a href="XUngrabKeyboard.html">XUngrabKeyboard</a>
   */
  public void ungrabKeyboard (int time) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (32, 0, 2);
      o.writeInt32 (time);
      o.send ();
    }
  }


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
  public void allowEvents (InputEvent evt, int time) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (35, evt.getCode(), 2);
      o.writeInt32 (time);
      o.send ();
    }
  }

  
  /** Reply of {@link #inputFocus()} */
  public class InputFocusInfo {

    public int revert_to;
    public int focus_id;
    public Window focus;

    InputFocusInfo (ResponseInputStream i) {
      revert_to = i.readInt8 ();
      i.skip (6);
      focus_id = i.readInt32 ();
      if (focus_id != 0 && focus_id != 1)
        focus = (Window) Window.intern (display, focus_id);
    }
  }
  
  
  // opcode 43 - get input focus
  /**
   * @see <a href="XGetInputFocus.html">XGetInputFocus</a>
   */
  public InputFocusInfo inputFocus () {

    InputFocusInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (43, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
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
  public byte [] queryKeymap () {
    RequestOutputStream o = display.getResponseOutputStream();
    byte [] data = new byte [32];
    synchronized (o) {
      o.beginRequest (44, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (8);
        i.readData (data);
      }
    }
    return data;
  }


  // opcode 100 - change keyboard mapping  
  /**
   * @see <a href="XChangeKeyboardMapping.html">XChangeKeyboardMapping</a>
   */
  public void changeKeyboardMapping (int firstKeycode, int keySymsPerKeycode,
                                     int [] keysyms) {
    
    int keycode_count = keysyms.length / keySymsPerKeycode;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (100, keycode_count, 2 + keysyms.length);
      o.writeInt8 (firstKeycode);
      o.writeInt8 (keySymsPerKeycode);
      o.skip (2);

      for (int i = 0; i < keysyms.length; i++)
        o.writeInt8 (keysyms [i]);

      o.send ();
    }
  }


  // opcode 101 - get keyboard mapping  
  /**
   * @see <a href="XGetKeyboardMapping.html">XGetKeyboardMapping</a>
   */
  public void keyboardMapping () {

    int keysym_count = maxKeycode - minKeycode + 1;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (101, 0, 2);
      o.writeInt8 (minKeycode);
      o.writeInt8 (keysym_count);
      o.writeInt16 (0); // Unused.

      ResponseInputStream in = display.getResponseInputStream();
      synchronized (in) {
        in.readReply (o);
        in.skip (1);
        keysymsPerKeycode = in.readInt8 ();
        in.skip (2); // Unused.
        int nm = in.readInt32 (); // length.
        assert nm == keysymsPerKeycode * keysym_count;
        in.skip (24); // Unused.
        keysyms = new int [nm];

        for (int i = 0; i < nm; i++) {
          keysyms [i] = in.readInt32 ();
        }
      }
    }
  }


  /** Reply of {@link #keyboardControl()} */
  public class KeyboardControlInfo {

    public boolean global_auto_repeat;
    
    private int led_mask;
    private int key_click_percent;
    private int bell_percent;
    private int bell_pitch;
    private int bell_duration;
    private byte[] auto_repeats;

    KeyboardControlInfo (ResponseInputStream i) {
      global_auto_repeat = i.readBool ();
      i.skip (6);
      led_mask = i.readInt32 ();
      key_click_percent = i.readInt8 ();
      bell_percent = i.readInt8 ();
      bell_pitch = i.readInt16 ();
      bell_duration = i.readInt16 ();
      i.skip (2);
      auto_repeats = new byte [32];
      i.readData (auto_repeats);
    }
  }
  
  
  /** X keyboard control. */
  public static class KeyboardControl extends ValueList { // TODO
    public KeyboardControl () { super (8); }
  
    public static final int OFF = 0;
    public static final int ON = 1;
  
  
    public static final String [] GLOBAL_AUTO_REPEAT_STRINGS = {"off", "on"};
  }


  // opcode 102 - change keyboard control
  /**
   * @see <a href="XChangeKeyboardControl.html">XChangeKeyboardControl</a>
   */
  public void changeKeyboardControl (KeyboardControl control) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (102, 0, 2 + control.count ());
      o.writeInt32 (control.bitmask);
      control.write (o);
      o.send ();
    }
  }


  // opcode 103 - get keyboard control
  /**
   * @see <a href="XGetKeyboardControl.html">XGetKeyboardControl</a>
   */
  public KeyboardControlInfo keyboardControl () {
    KeyboardControlInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (103, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
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
  public void changePointerControl (boolean do_accel, boolean do_threshold,
                                      int accel_numerator,
                                      int accel_denominator, int threshold) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (105, 0, 3);
      o.writeInt16 (accel_numerator);
      o.writeInt16 (accel_denominator);
      o.writeInt16 (threshold);
      o.writeBool (do_accel);
      o.writeBool (do_threshold);
      o.send ();
    }
  }


  /** Reply of {@link #pointerControl()}. */
  public class PointerControlInfo {

    public int acceleration_numerator;
    public int acceleration_denominator;
    public int treshold;

    PointerControlInfo (ResponseInputStream i) {
      acceleration_numerator = i.readInt16 ();
      acceleration_denominator = i.readInt16 ();
      treshold = i.readInt16 ();
    }
  }
  
  
  // opcode 106 - get pointer control
  /**
   * @see <a href="XGetPointerControl.html">XGetPointerControl</a>
   */
  public PointerControlInfo pointerControl () {
    PointerControlInfo info;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (106, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (8);
        info = new PointerControlInfo (i);
        i.skip(18);
      }
    }
    return info;
  }

  // opcode 116 - set pointer mapping
  /**
   * @return valid:
   * {@link #SUCCESS},
   * {@link #BUSY}
   *
   * @see <a href="XSetPointerMapping.html">XSetPointerMapping</a>
   */
  public Status setPointerMapping (byte [] map) {

    int n = map.length;
    int p = RequestOutputStream.pad (n);

    int status;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (116, map.length, 1 + (n + p) / 4);
      o.writeBytes (map);
      o.skip (p);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (1);
        status = i.readInt8 ();
        i.skip (30);
      }
    }
    return Status.getStatus(status);
  }


  // opcode 117 - get pointer mapping
  /**
   * @see <a href="XGetPointerMapping.html">XGetPointerMapping</a>
   */
  public byte [] getPointerMapping () {

    byte [] map;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (117, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (1);
        int len = i.readInt8 ();
        i.skip (30);
        map = new byte [len];
        i.readData (map);
      }
    }
    return map;
  }

  // opcode 118 - set modifier mapping
  /**
   * @return valid:
   * {@link #SUCCESS},
   * {@link #BUSY},
   * {@link #FAILED}
   *
   * @see <a href="XSetModifierMapping.html">XSetModifierMapping</a>
   */
  public Status setModifierMapping (int keycodesPerModifier, byte [] keycodes) {

    int status;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (118, keycodesPerModifier,
                       1 + 2 * keycodesPerModifier);
      o.write (keycodes);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (1);
        status = i.readInt8 ();
        i.skip (30);
      }
    }
    return Status.getStatus(status);
  }

  public class ModifierMapping {

    public int keycodesPerModifier;
    byte [] map;

    ModifierMapping (ResponseInputStream i) {
      keycodesPerModifier = i.readInt8 ();
      i.skip (30);
      map = new byte [keycodesPerModifier * 8];
      i.readData (map);
    }
  }

  // opcode 119 - get modifier mapping
  /**
   * @return valid: {@link Enum#next1()}
   * @see <a href="XModifierKeymap.html">XModifierKeymap</a>
   */
  public ModifierMapping modifierMapping () {

    ModifierMapping map;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (119, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized (i) {
        i.readReply (o);
        i.skip (1);
        map = new ModifierMapping (i);        
      }
    }
    return map;
  }

// FIXME: CAN BE REMOVED?
//  public static final String [] KEYBUT_STRINGS = {
//    "shift", "lock", "control", "mod1", "mod2", "mod3", "mod4", "mod5",
//    "button1", "button2", "button3", "button4", "button5"
//  };

//FIXME: CAN BE REMOVED?
//  public static void dump_keybut_mask (int m) {
//    for (int i=0; i<KEYBUT_STRINGS.length; i++)
//      if (((m & 0x1fff) & 1 << i) != 0)
//	System.out.print (KEYBUT_STRINGS [i] + " ");
//  }

  /**
   * Maps a keycode to a keysym.
   *
   * @param keycode the keycode
   * @param keystate the modifiers
   *
   * @return the keysym for the specified key code and modifier mask
   */
  public int keycodeToKeysym (int keycode, int keystate) {
    return keycodeToKeysym(keycode, keystate, false);
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
  public int keycodeToKeysym (int keycode, int keystate,
                                boolean ignore_modifiers) {
    if (keycode > maxKeycode) 
      throw new java.lang.Error ("Invalid keycode: " + keycode);

    int keysym = 0;
    int keysym_no = 0;

    // TODO: Maybe add handling of other modifiers.
    if (! ignore_modifiers) {
      if ((keystate & KeyMask.SHIFT_MASK.getCode()) != 0)
        keysym_no = 1;
      else if ((keystate & KeyMask.MOD5_MASK.getCode()) != 0) // Alt Gr
        keysym_no = 2; // TODO: 4 seems also valid.
    }

    int index = (keycode - minKeycode) * keysymsPerKeycode + keysym_no;
    keysym = keysyms[index];
    return keysym;
  }


  public int keysymToKeycode (int keysym) {
    // linear lookup - expensive?
    //for (int i=0; i<keysyms.length; i++)
    // FIXME, hacked to do it in reverse order for solaris

    for (int i=keysyms.length-1; i>=0; i--)
      if (keysyms [i] == keysym)
	return i + minKeycode;
    
    throw new java.lang.Error ("Invalid keysym: " + keysym);	
  }


  /** 
   * Input#ungrab_keyboard(int)
   */
  public void ungrabKeyboard () {
    ungrabKeyboard (Display.CURRENT_TIME);
  }
}
