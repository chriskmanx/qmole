package gnu.app.puppet;

import gnu.x11.*;
import gnu.x11.event.*;
import gnu.x11.Input;           // shadow gnu.x11.event.Input
import gnu.x11.Error;           // shadow java.lang.Error
import gnu.x11.extension.XTest;
import gnu.x11.extension.NotFoundException;
import gnu.x11.keysym.Misc;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;


/**
 * Window manager.
 *
 *
 * 
 * <p>In my PC 104 keyboard system,
 * <pre>
 *   META (MOD1) = "alt" next to spacebar 
 *   ALT (MOD4) = "window" next to meta
 *   SUPER (MOD3) = "menu" next to right CONTROL
 * </pre>
 * Please see <a href="../../../etc/Xmodmap">Xmodmap</a>.
 *
 * 
 *
 * <h3>Bindings</h3>
 *
 * <table border>
 * <tr><th>KEY SEQUENCE</th><th>COMMAND</th></tr>
 *
 *
 * <tr><th>basic</th></tr>
 * <tr><td>[enter]</td><td>finish</td><tr>
 * <tr><td>[kp-enter]</td><td>finish</td><tr>
 * <tr><td>[escape]</td><td>finish or abort-switch-focus</td><tr>
 * <tr><td>[(control delete) (meta DIGIT)]</td>
 * <td>lanuch-command DIGIT</td><tr>
 * <tr><td>[(control delete) S]</td><td>*search-window-backward</td><tr>
 * <tr><td>[(control delete) s]</td><td>*search-window-forward</td><tr>
 *
 * 
 * <tr><th>focus</th></tr>
 * <tr><td>[(meta tab)]</td><td>switch-focus-ignore-class-forward</td><tr>
 * <tr><td>[(shift meta tab)]</td>
 * <td>switch-focus-ignore-class-backward</td><tr>
 * <tr><td>[(alt tab)]</td><td>switch-focus-same-class-forward</td><tr>
 * <tr><td>[(shift alt tab)]</td>
 * <td>switch-focus-same-class-backward</td><tr>
 * <tr><td>[(super tab)]</td>
 * <td>switch-focus-different-class-forward</td><tr>
 * <tr><td>[(super tab)]</td>
 * <td>switch-focus-different-class-backward</td><tr>
 *
 * 
 * <tr><th>window</th></tr>
 * <tr><td>[(control delete) ARROW]</td><td>*move</td><tr>
 * <tr><td>[(control delete) (shift ARROW)]</td><td>*move faster</td><tr>
 * <tr><td>[(control delete) (control ARROW)]</td><td>*resize</td><tr>
 * <tr><td>[(control delete) (control ARROW)]</td>
 * <td>*resize faster</td><tr>
 * <tr><td>[(control delete) pageup]</td><td>raise</td><tr>
 * <tr><td>[(control delete) page-down]</td><td>lower</td><tr>
 * <tr><td>[(control delete) d]</td><td>!dump-info</td><tr>
 * <tr><td>[(control delete) f]</td><td>toggle-focus</td><tr>
 * <tr><td>[(control delete) F]</td><td>grant-all-focus</td><tr>
 * <tr><td>[(control delete) h]</td><td>!hide-which</td><tr>
 * <tr><td>[(control delete) H]</td><td>!unhide-which</td><tr>
 * <tr><td>[(control delete) m]</td><td>minimize</td><tr>
 * <tr><td>[(control delete) M]</td><td>!maximize</td><tr>
 * <tr><td>[(control delete) r]</td><td>restore-size</td><tr>
 * <tr><td>[(control delete) R]</td><td>save-size</td><tr>
 * <tr><td>[(control delete) w]</td><td>!warp-pointer</td><tr>
 * <tr><td>[(control delete) z]</td><td>*relocate</td><tr>
 * 
 *
 * <tr><th>register</th></tr>
 * <tr><td>[(control delete) space KEY]</td>
 * <td>client-to-register KEY</td><tr>
 * <tr><td>[(control delete) j KEY]</td><td>jump-to-register KEY</td><tr>
 * <tr><td>[(control delete) (control DIGIT)]</td>
 * <td>jump-to-register DIGIT</td><tr> 
 *
 * 
 * <tr><th>critical</th></tr>
 * <tr><td>[(control delete) backspace d]</td><td>delete-window</td><tr>
 * <tr><td>[(control delete) backspace k]</td><td>kill-window</td><tr>
 * <tr><td>[(control delete) backspace Q]</td><td>quit-puppet</td><tr>
 *
 * 
 * <tr><th>geometry</th></tr>
 * <tr><td>[(control delete) g ARROW]</td><td>*gravitate<td><tr>
 * <tr><td>[(control delete) g (shift ARROW)]</td>
 * <td>*gravitate-absolute<td><tr>
 * <tr><td>[(control delete) g ?2]</td><td>*scale-two-third<td><tr>
 * <tr><td>[(control delete) g ?3]</td><td>*scale-one-and-half<td><tr>
 * <tr><td>[(control delete) g ?d]</td><td>*scale-double<td><tr>
 * <tr><td>[(control delete) g ?h]</td><td>*scale-half<td><tr>
 *
 * 
 * <tr><th>mouse</th></tr>
 * <tr><td>[(meta BUTTON) on root]</td><td>lanuch-on-root</td><tr>
 * <tr><td>[BUTTON on root]</td><td>pointer-root-focus</td><tr>
 * <tr><td>[(control button1)]</td><td>focus-with-raise</td><tr>
 * <tr><td>[(control button2)]</td><td>focus-without-raise</td><tr>
 * <tr><td>[(control button3)]</td><td>lower-behind</td><tr>
 * <tr><td>[(control meta button1)]</td><td>delete-window</td><tr>
 * <tr><td>[(control meta button3)]</td><td>kill-window</td><tr>
 *
 *
 * <tr><th>mouse key</th></tr>
 * <tr><td>[(control delete) KP_ARROW]</td><td>*move-pointer</td><tr>
 * <tr><td>[(control delete) (shift KP_ARROW)]</td>
 * <td>*move-pointer faster</td><tr>
 * <tr><td>[(control delete) (control shift KP_ARROW)]</td>
 * <td>*move-pointer even faster</td><tr>
 * <tr><td>[(control delete) kp-begin]</td><td>click-button1</td><tr>
 * <tr><td>[(control delete) (shift kp-begin)]</td>
 * <td>double-click-button1</td><tr>
 * <tr><td>[(control delete) kp-divide]</td><td>click-button1</td><tr>
 * <tr><td>[(control delete) (shift kp-divide)]</td>
 * <td>double-click-button2</td><tr>
 * <tr><td>[(control delete) kp-multiply]</td><td>click-button1</td><tr>
 * <tr><td>[(control delete) (shift kp-multiply)]</td>
 * <td>double-click-button3</td><tr>
 * </table>
 *
 * 
 * <dl compact>
 * <dt>*
 * <dd>persistent command
 * 
 * <dt>!
 * <dd>command accepting argument
 *
 * <dt>ARROW
 * <dd>up, down, left, or right
 *
 * <dt>BUTTON
 * <dd>BUTTON1, BUTTON2, BUTTON3
 *
 * <dt>DIGIT
 * <dd>0, 1, 2, 3, 4, 5, 6, 7, 8, or 9
 * 
 * <dt>KEY
 * <dd>any key (including modifier)
 *
 * <dt>KP_ARROW
 * <dd>kb_up, kb_down, kb_left, kb_right, kb_home, kb_end, kb_pageup,
 * kb_pagedown
 * </dl>
 *
 *
 *
 * <h3>Persistent Command</h3>
 * 
 * There are two types of commands: <em>persistent</em> commands, and
 * <em>non-persistent</em> commands. Non-persistent commands such as
 * `delete-window' will exit active keyboard state (ungrab keyboard) when
 * their actions are executed. On the other hand, persistent commands such
 * as `relocate' will stay in active keyboard state (do not ungrab
 * keyboard) even when their actions are executed. This feature allows
 * users execute similar commands in a sequence without repeatively
 * pressing the system key.
 *
 * <p>Note, however, there is no visual feedback for persistent commands. 
 * Users may feel their keyboards are locked, while actually this window
 * manager is expecting more keystrokes. Remember to press `finish' key to
 * finish a persistent command.
 *
 *
 *
 * <h3>Argument</h3>
 * A prefix argument mechanism similar to that in Emacs is employed as
 * follows. After pressing system key and before pressing the command key,
 * pressing any digit (or '-' for negative) will become the argument for
 * the command. There can be more than one digits, which will be formed as
 * an integer argument in decimal.
 *
 * <table>
 * <tr><th>COMMAND</th><th>ARGUMENT</th><th>DESCRIPTION</th></tr>
 * <tr><td>dump-info</td><td>nothing</td><td>dump-basic-info</td></tr>
 * <tr><td></td><td>negative</td><td>dump-hidden-windows</td></tr>
 * <tr><td></td><td>other</td><td>dump-all-windows</td></tr>
 * 
 * <tr><td>hide-which</td><td>nothing</td><td>hide-focus</td></tr>
 * <tr><td></td><td>negative</td><td>hide-others</td></tr>
 * <tr><td></td><td>other</td><td>hide-same-class</td></tr>
 * 
 * <tr><td>unhide-which</td><td>nothing</td><td>undo-hide</td></tr>
 * <tr><td></td><td>negative</td><td>unhide-all</td></tr>
 * <tr><td></td><td>other</td><td>unhide-same-class</td></tr>
 * 
 * <tr><td>maximize</td><td>nothing</td><td>maximize-user-space</td></tr>
 * <tr><td></td><td>other</td><td>maximize-full-screen</td></tr>
 * </table>
 *
 * 
 *
 * <h3>Customization</h3>
 *
 * There aren't much to customized about! In fact, you have to edit Java
 * source code and recompile to customize anything at all. Check
 * <code>gnu/app/puppet/Preference.java</code> and <code>key_process_*()</code> in
 * <code>gnu/app/puppet/Puppet.java</code>.
 *
 * 
 * <h3>Focus Policy - Keyboard</h3>
 * 
 * <b>Basic switch-focus</b> ([(meta tab)] and [(meta shift tab)]) (similar
 * to Microsoft Windows (tm)): to change focus to the most recently used
 * normal window, press [(meta tab)]; to change focus to the least recently
 * used normal window, press [(meta shift tab)]. To navigate all normal
 * windows, continuously press and release TAB while holding META. Normal
 * windows are those in state {@link #NORMAL} which is the
 * default state for freshly mapped windows. Windows that are hidden or
 * denied focus by user, or unmapped by application are not considered
 * normal.
 *
 * <p><b>Class-based switch-focus</b> ([(alt tab)] and [(super tab)]):
 * they are exactly the same as [(meta tab)] counterparts, except that the
 * selection criteria are WM-CLASS sensitive. WM-CLASS is an classification
 * of applications defined in icccm 4.1.2.5. While [(alt tab)] considers
 * all normal windows, WINDOW-TAB considers normal windows that are of the
 * same WM_CLASS as the window under current focus; and [(super tab)]
 * considers those of different WM_CLASS. Note pressing SHIFT will reverse
 * the direction.
 *
 * <p><b>Find Window</b> (`search-window-forward' and
 * `search-window-backward'): you can search and focus a window by pressing
 * its first character of its WM-CLASS string. To go to next matching
 * window, press the same character again, for as many times as you want. 
 * Press [enter] or [escape] when done (as it is a persistent command). To
 * search among all managed windows (including normal, hidden and
 * focus-denied windows), hold SHIFT while pressing the character, that is,
 * press its corresponding capital letter.
 *
 * <p><b>Register</b> (`client-to-register' and `jump-to-register')
 * (similar to Emacs buffer register): you can store a window to a
 * register, and then later restore the focus to that window in the
 * register. A register is any key (including control, alt, shift....!) on
 * the keyboard. `client-to-register stores a window to a register, while
 * `jump-to-register' later will unhide and raise (if necessary) the window
 * in the register, and then give it the input focus.
 *
 * <p><b>Fall-back</b>: this policy decides which window to focus if the
 * current window is being hidden, unmapped, or denied focus. Unlike
 * Microsoft Windows (tm) which unhelpfully gives the focus back to
 * nowhere, focus will be fallen back to the most recently used normal
 * window (as if [(meta tab)] is pressed).
 *
 * 
 *
 * <h3>Focus Policy - Mouse</h3>
 * 
 * A policy similar to CLICK-TO-FOCUS in other window managers is
 * employed as follows. To raise and focus a window, press BUTTON1 while
 * holding CONTROL; to focus without raising, press BUTTON2 while holding
 * CONTROL; to lower and window and give focus to some other window
 * (happens to contain the pointer after re-stacking), press BUTTON3 while
 * holding CONTROL.
 * 
 * <p>Intercepting button press events is discouraged (prohibited?) in icccm,
 * but there is not much choice as we don't do re-parenting.
 * 
 * 
 * 
 * <h3>Mouse Key</h3>
 * 
 * A simple mechanism is employed to support moving the pointer with
 * keyboard. Please check the above section, "Binding", for details.
 * 
 * <p>Supposedly, X server comes with XKB extension that supports MouseKey
 * as a built-in feature. However, personally I find it inconvenient to use
 * (num-lock for on/off and default click keys) and not highly customizable
 * (acceleration time and delay). Moreover, there is a serious bug in
 * reference implementations of X server as pointed out by Stephen
 * Montgomery-Smith (stephen@math.missouri.edu) <a href=
 * "http://www.math.missouri.edu/~stephen/software/bugfix-for-mousekeys">
 * here</a>. He also wrote some customization tool (<a href=
 * "http://www.math.missouri.edu/~stephen/software/#xkbset"> Xkkset</a>)
 * for many XKB features. In the meantime, Puppet supports its own mouse
 * key binding as a supplementary mechanism.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/puppet/Puppet.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/puppet/Puppet.help">
 * help output</a>
 */
public class Puppet extends Application {
  public static final int SYSTEM_MODIFIER = Input.CONTROL_MASK;
  public static final int SYSTEM_KEYSYM = Misc.DELETE;
  public static final int SWITCH_KEYSYM = Misc.TAB;


  // internal state
  public static final int UNMANAGED = 0;
  public static final int NORMAL = 1;
  public static final int HIDDEN = 2;
  public static final int NO_FOCUS = 3;


  // delta for resize and relocate
  public static final int DELTA_LARGE = 20;
  public static final int DELTA_SMALL = 2;


  /** see Focus Policy - Keyboard. */
  public Client [] registers;


  /** 
   * A list of all managed children of root window, that is, our clients. 
   * We need to frequently remove and add elements to maintain focus order
   * - any better/faster data structure?
   *
   * @see #scan_children()
   */
  public Vector clients = new Vector ();


  /** 
   * The active window when FOCUS-KEY (ALT-TAB, WINDOW-TAB, or MENU-TAB)
   * is pressed. Used to aid prevent looping and maintain focus order.
   *
   * @see #next_client(Client, boolean)
   */
  public Client focus_base;
  public Vector focus_so_far = new Vector ();
  

  // useful atoms (from twm), not all implemented yet
  Atom _mit_priority_colors, wm_change_state, wm_state,
    wm_colormap_windows, wm_protocols, wm_take_focus, wm_save_yourself,
    wm_delete_window, sm_client_id, wm_client_leader, wm_window_role;


  public Preference pref = new Preference ();
  public Rectangle space;
  public Window root;           // just an alias
  public Client focus, last_hide;
  public XTest xtest;
  public boolean print_event;


  public Puppet (String [] args) throws NotFoundException {
    super (args);

    print_event = option.booleann ("print-event",
      "dump all events for debug", false);

    space = option.rectangle ("space",
      "workspace for normal windows", pref.space ());

    about ("0.1", "puppet window manager",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/", 
      "\nFor bindings, check http://escher.sourceforge.net/"
      + "current/doc/gnu/app/puppet/Puppet.html.");

    if (help_option) return;

    /**
     * addShutdownHook in IBM JDK 1.3 does not seem to work for processing
     * Control-C. It simply ignores TERM signal instead of calling shutdown
     * hooks and then exiting.
     */
//     Runtime.getRuntime ().addShutdownHook (new Thread () {
//       public void run () { System.err.println ("shutting down..."); }});

    root = display.default_root;
    Window.NONE.display = display; // for move pointer
    xtest = new XTest (display); // for press button
    registers = new Client [display.input.max_keycode 
      - display.input.min_keycode]; 

    _mit_priority_colors = (Atom) Atom.intern (display, "_MIT_PRIORITY_COLORS");
    wm_change_state = (Atom) Atom.intern (display, "WM_CHANGE_STATE");
    wm_state = (Atom) Atom.intern (display, "WM_STATE");
    wm_colormap_windows = (Atom) Atom.intern (display, "WM_COLORMAP_WINDOWS");
    wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
    wm_take_focus = (Atom) Atom.intern (display, "WM_TAKE_FOCUS");
    wm_save_yourself = (Atom) Atom.intern (display, "WM_SAVE_YOURSELF");
    wm_delete_window = (Atom) Atom.intern (display, "WM_DELETE_WINDOW");
    sm_client_id = (Atom) Atom.intern (display, "SM_CLIENT_ID");
    wm_client_leader = (Atom) Atom.intern (display, "WM_CLIENT_LEADER");
    wm_window_role = (Atom) Atom.intern (display, "WM_WINDOW_ROLE");

    control_root_window ();
    scan_children ();

    focus = (Client) Client.intern (root.query_pointer ().child);
    focus.set_input_focus ();
    
    grab_keybut ();
    System.out.println ("Initialization completed.");

    while (!exit_now) read_and_dispatch_event ();    
  }


  public void alert_user (String message) {
    System.out.println (message);
    display.bell (-50);		// not too loud, please
  }


  public void client_to_register (Client client, int keycode) {
    int index = client.register = keycode - display.input.min_keycode;
    Client old = registers [index];

    if (old != null) old.register = -1;
    registers [index] = client;
  }


  public void control_root_window () {
    try {
      root.select_input (
        Event.BUTTON_PRESS_MASK

	// unmap, destroy notify
	| Event.SUBSTRUCTURE_NOTIFY_MASK

	// map, configure, circulate request
	| Event.SUBSTRUCTURE_REDIRECT_MASK

	// icccm properties (wm name, hints, normal hints)
	| Event.PROPERTY_CHANGE_MASK);

      display.check_error ();
      
    } catch (Error e) {
      if (e.code == Error.BAD_ACCESS && e.bad == root.id)
	throw new RuntimeException (
          "Failed to access root window\nAnother WM is running?");
      else
	throw e;
    }
  }


  public void deny_focus (Client client) {
    client.state = NO_FOCUS;
    give_up_focus (client);
  }


  public void focus_client_first_char (Client client, boolean reverse,
    char c, boolean all) {

    // support only alphabetic window class
    if (c < 'A' || c > 'z') return;

    Client next = next_client_first_char (client, reverse, c, all);
    if (next == null) alert_user ("Client not found: " + c);

    // possibly hidden and no-focus, as we search all windows
    // BUT do not grant focus, leave it as before
    if (all && next != null) unhide (next);

    set_focus (next, true);
  }


  public void give_up_focus (Client client) {
    if (client == focus) 
      set_focus (next_client_normal (client, true), false);
  }


  public void grab_keybut () {
    // system key
    root.grab_key_ignore_locks (SYSTEM_KEYSYM, SYSTEM_MODIFIER, true,
      Window.ASYNCHRONOUS, Window.ASYNCHRONOUS);

    // rotate to next normal window - ignore class
    root.grab_key_ignore_locks (SWITCH_KEYSYM, Input.META_MASK, true,
      Window.ASYNCHRONOUS, Window.ASYNCHRONOUS);
    root.grab_key_ignore_locks (SWITCH_KEYSYM,
      Input.META_MASK|Input.SHIFT_MASK, true, Window.ASYNCHRONOUS,
      Window.ASYNCHRONOUS);

    // rotate to next normal window - different class 
    root.grab_key_ignore_locks (SWITCH_KEYSYM, Input.SUPER_MASK, true, 
      Window.ASYNCHRONOUS, Window.ASYNCHRONOUS);
    root.grab_key_ignore_locks (SWITCH_KEYSYM,
      Input.SUPER_MASK|Input.SHIFT_MASK, true, Window.ASYNCHRONOUS,
      Window.ASYNCHRONOUS);

    // rotate to next normal window - same class
    root.grab_key_ignore_locks (SWITCH_KEYSYM, Input.ALT_MASK, 
      true, Window.ASYNCHRONOUS, Window.ASYNCHRONOUS);
    root.grab_key_ignore_locks (SWITCH_KEYSYM,
      Input.ALT_MASK|Input.SHIFT_MASK, true, Window.ASYNCHRONOUS,
      Window.ASYNCHRONOUS);

    // CLICK-TO-FOCUS
    root.grab_button_ignore_locks (Window.ANY_BUTTON,
      Input.CONTROL_MASK, true, Event.BUTTON_PRESS_MASK,
      Window.ASYNCHRONOUS, Window.ASYNCHRONOUS, Window.NONE,
      Cursor.NONE);

    // close / delete
    root.grab_button_ignore_locks (Window.ANY_BUTTON,
      Input.CONTROL_MASK | Input.META_MASK, true,
      Event.BUTTON_PRESS_MASK, Window.ASYNCHRONOUS, Window.ASYNCHRONOUS,
      Window.NONE, Cursor.NONE);    
  }


  public void grant_all_focus () {
    for (Iterator it=clients.iterator (); it.hasNext ();) {
      grant_focus ((Client) it.next ());
    }
  }


  public void grant_focus (Client client) {
    if (client.state == NO_FOCUS) client.state = NORMAL;
  }


  public boolean grant_preference (Client client) {
    if (client.class_hint == null) return false;

    String id = client.class_hint.res + ":";
    Object value;


    // no focus
    if (pref.no_focus (id)) client.state = Puppet.NO_FOCUS;


    // register
    int keysym = '0' + pref.register (id);
    if (keysym >= '0') {
      int keycode = display.input.keysym_to_keycode (keysym);
      client.register = keycode - display.input.min_keycode;
      registers [client.register] = client;
    }


    int max_width = display.default_screen.width;
    int max_height = display.default_screen.height;


    // position      
    Rectangle r = pref.position (id);
    if (r != null) {
      // similar to geometry parameter processing in X toolkit
      r.x = (r.x + max_width) % max_width;
      r.y = (r.y + max_height) % max_height;
      client.move_resize (r);
      return true;		// user-specified position
    }


    return false;
  }


  public void hide (Client client) {
    if (client.state == HIDDEN) return;

    /* Set this state to give hint to {@link
     * #when_unmap_notify(UnmapNotify)}.
     *
     * <p>Do it before <code>client.unmap ()</code>. 
     */
    client.state = HIDDEN;
    client.set_wm_state (Window.WMState.ICONIC);

    client.unmap ();
    last_hide = client;
  }


  public void hide_others (Client client) {
    for (Iterator it=clients.iterator (); it.hasNext ();) {
      Client c = (Client) it.next ();

      if (c.state == NORMAL && c != client) hide (c);
    }
  }


  public void hide_same_class (Client client) {
    for (Iterator it=clients.iterator (); it.hasNext ();) {
      Client c = (Client) it.next ();

      if (c.state == NORMAL
        && c.class_hint != null
	&& c.class_hint.class_equals (client.class_hint))

	hide (c);
    }
  }


  public void jump_to_register (int keycode) {
    int index = keycode - display.input.min_keycode;
    Client client = registers [index];

    if (client != null) {
      // do not grant focus, leave it as before
      unhide (client);		// de-iconify, if neceesary
      set_focus (client, true);

    } else alert_user ("Register not defined: " + keycode); 
  }


  public boolean key_do_change_geometry () {
    int delta = shift_down ? 0 : DELTA_LARGE;

    switch (keysym) {
    case Misc.LEFT:            // `gravitate' or `gravitate-absolute'
      focus.x = space.x + delta;
      focus.move ();
      return false;

    case Misc.RIGHT:
      focus.x = space.x
        + space.width - focus.width - delta;
      focus.move ();
      return false;

    case Misc.UP:
      focus.y = space.y + delta;
      focus.move ();
      return false;

    case Misc.DOWN:
      focus.y = space.y
        + space.height - focus.height - delta;
      focus.move ();
      return false;

    case '2':			// `scale-two-third'
      scale_size (focus, 0.67);
      return false;

    case '3':			// `scale-one-and-half'
      scale_size (focus, 1.5);
      return false;

    case 'd':			// `scale-double'
      scale_size (focus, 2.0);
      return false;

    case 'h':			// `scale-half'
      scale_size (focus, 0.5);
      return false;

    default:
      alert_user ("Undefined keysym for 'change geometry': " + keysym);
      return false;
    }
  }


  public boolean key_do_critical_operation () {
    switch (keysym) {
    case 'd':                   // `delete-window'
      focus.delete ();
      return true;

    case 'k':                   // `kill-window'
      focus.kill ();
      return true;

    case 'Q':                   // `quit-puppet'
      when_quit ();
      exit ();
      return true;       

    default:
      alert_user ("Undefined keysym for 'critical operation': " + keysym);
      return false;
    }
  }


  public boolean key_do_jump_lanuch_or_argument () {
    if (control_down) {         // `jump-to-register DIGIT'
      jump_to_register (keycode);
      return true;

    } else if (meta_down) {	// `lanuch-command'
      String command = pref.launch (keysym - '0');
      
      if (command == null) {
        alert_user ("Command not defined: " + (keysym - '0'));
        return true;
      }

      try {
        Runtime.getRuntime ().exec (command);
      } catch (java.io.IOException e) {
        alert_user ("Failed to launch " + command + ": " + e);
      }

      return true;
       
    } else {			// argument
      argument_present = true;

      if (keysym == '-') {
        argument_negative = true;
        return false;
      }
      
      int i = ((char) keysym) - '0';
      argument = argument * 10 + i;
      return false;
    }
  }


  public void key_dump_info () {
    System.out.println ("input focus: " + focus);
    System.out.println ("mouse at: " + root.query_pointer ().root_position ());

    if (!argument_present) return; // `dump-basic-info'

    if (argument_negative) {    // `dump-hidden-windows'"
      System.out.println ("all hidden clients: ");

      for (Iterator it=clients.iterator (); it.hasNext ();) {
        Client c = (Client) it.next ();
        
        if (c.state == HIDDEN)
          System.out.println (c);
      }

    } else                      // `dump-all-windows'
      System.out.println ("all clients: " + clients);
  }


  public void key_hide_which () {
    if (!argument_present)      // `hide-focus'
      hide (focus);
    else if (argument_negative) // `hide-others'
      hide_others (focus);
    else                        // `hide-same-class'
      hide_same_class (focus);
  }


  public void key_unhide_which () {
    if (!argument_present)      // `undo-hide'
      unhide (last_hide);
    else if (argument_negative) // `unhide-all'
      unhide_all ();
    else                        // `unhide-same-class'
      unhide_same_class (focus);
  }


  public void key_move_or_resize (int x_direction, int y_direction) {
    // TODO send synthetic ConfigureNotify?
    int delta = shift_down ? DELTA_LARGE : DELTA_SMALL;

    if (control_down) {         // `resize'
      focus.width += x_direction * delta;
      focus.height += y_direction * delta;
      focus.resize ();

    } else {                    // `move'
      focus.x += x_direction * delta;
      focus.y += y_direction * delta;
      focus.move ();
    }
  }


  public void key_move_pointer (int x_direction, int y_direction) {
    // `move-pointer'
    int scale = (shift_down ? DELTA_LARGE : DELTA_SMALL)
      * (meta_down ? DELTA_LARGE : DELTA_SMALL);

    int x = x_direction * scale;
    int y = y_direction * scale;
    Window.NONE.warp_pointer (x, y);
  }


  public boolean key_process () {
    /* These are global to cases with any prefix.
     *
     * RETURN, KP_ENTER, and ESCAPE will terminate key press sequence and
     * ungrab keyboard in all circumstances.
     *
     * Lock modifiers (SHIFT, CONTROL, META, ALT, SUPER, and HYPER) will be
     * ignored if they are pressed without other keys, except when defining
     * or jumping to register.
     */
    switch (keysym) {
    case Misc.RETURN:		// `finish'
    case Misc.KP_ENTER:
    case Misc.ESCAPE:
      return true;

    case Misc.SHIFT_L:         // ignore
    case Misc.SHIFT_R:
    case Misc.CONTROL_L:
    case Misc.CONTROL_R:
    case Misc.META_L:
    case Misc.META_R:
    case Misc.ALT_L:
    case Misc.ALT_R:
    case Misc.SUPER_L:
    case Misc.SUPER_R:
    case Misc.HYPER_L:
    case Misc.HYPER_R:
      if (prefix0 != ' ' && prefix0 != 'j') return false;		
    }


    if (prefix0 == 0) return key_process_no_prefix ();
    if (prefix1 == 0) return key_process_prefix0 ();
    return key_process_prefix1 ();
  }


  public boolean key_process_no_prefix () {
    switch (keysym) {
    case Misc.BACKSPACE:	// prefix of critical operation
    case ' ':			// prefix of client to register
    case 'j':			// prefix of jump to register
    case 'g':			// prefix of change geometry
    case 's':			// prefix of search client
    case 'S':			// prefix of search client
      prefix0 = keysym;
      return false;

    case Misc.PAGE_UP:         // `raise'
      focus.raise ();
      return true;

    case Misc.PAGE_DOWN:       // `lower'
      focus.lower ();
      return true;

    case Misc.LEFT:
      key_move_or_resize (-1, 0);
      return false;

    case Misc.RIGHT:
      key_move_or_resize (1, 0);
      return false;

    case Misc.UP:
      key_move_or_resize (0, -1);
      return false;

    case Misc.DOWN:
      key_move_or_resize (0, 1);
      return false;

    case Misc.KP_HOME:
      key_move_pointer (-1, -1);
      return false;
      
    case Misc.KP_UP:
      key_move_pointer (0, -1);
      return false;
      
    case Misc.KP_PAGE_UP:
      key_move_pointer (1, -1);
      return false;
      
    case Misc.KP_LEFT:
      key_move_pointer (-1, 0);
      return false;
      
    case Misc.KP_RIGHT:
      key_move_pointer (1, 0);
      return false;
      
    case Misc.KP_END:
      key_move_pointer (-1, 1);
      return false;
      
    case Misc.KP_DOWN:
      key_move_pointer (0, 1);
      return false;

    case Misc.KP_PAGE_DOWN:
      key_move_pointer (1, 1);
      return false;

    case Misc.KP_BEGIN:
      key_click_button (Input.BUTTON1);
      return true;

    case Misc.KP_DIVIDE:
      key_click_button (Input.BUTTON2);
      return true;

    case Misc.KP_MULTIPLY:
      key_click_button (Input.BUTTON3);
      return true;
            
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '0':
    case '-':
      return key_do_jump_lanuch_or_argument ();

    case 'd':                   // `dump-info'
      key_dump_info ();
      return true;

    case 'f':                   // `toggle-focus'
      toggle_focus (focus);
      return true;
      
    case 'F':                   // `grant-all-focus'
      grant_all_focus ();
      return true;
      
    case 'h':                   // `hide-which'
      key_hide_which ();
      return true;

    case 'H':                   // `unhide-which'
      key_unhide_which ();
      return true;

    case 'm':                   // `minimize'
      minimize (focus);
      return true;

    case 'M':
      // `maximize', `maximize-user-space', `maximize-full-screen'
      maximize (focus, argument_present);
      return true;

    case 'r':                   // `restore-size'
      restore_size (focus);
      return true;

    case 'R':                   // `save-size'
      save_size (focus);
      return true;

    case 'w':                   // `warp-pointer'
      key_warp_pointer ();
      return true;

    case 'z':
      relocate (focus);
      return false;     

    default:
      alert_user ("Undefined keysym: " + keysym);
      return false;
    }
  }


  public boolean key_process_prefix0 () {
    switch (prefix0) {
    case Misc.BACKSPACE:
      return key_do_critical_operation ();

    case ' ':                   // `client-to-register KEY'
      client_to_register (focus, keycode);
      return true;

    case 'j':                   // `jump-to-register KEY'
      jump_to_register (keycode);
      return true;

    case 'g':
      return key_do_change_geometry ();

    case 'S':                   // search-window-backward
    case 's':                   // search-window-forward
      key_search_client ();
      return false;

    default:
      throw new java.lang.Error ("Unhandled prefix0: " + prefix0);
    }
  }


  public boolean key_process_prefix1 () {
    throw new java.lang.Error ("Unhandled prefix1: " + prefix1);
  }



  public void key_click_button (int button) {
    /* Click = Press + Release. We must fake both ButtonPress and
     * ButtonRelease, or some programs would think we are dragging.
     */

    // `click-button1'
    // `click-button2'
    // `click-button3'
    xtest.fake_button_event (button, true, 0);
    xtest.fake_button_event (button, false, 0);


    // `double-click-button1'
    // `double-click-button2'
    // `double-click-button3' 
    if (shift_down) {
      xtest.fake_button_event (button, true, 0);
      xtest.fake_button_event (button, false, 0);
    }
  }


  public void key_search_client () {
    focus_client_first_char (focus, false, (char) keysym, shift_down);
  }


  public void key_switch_focus () {
    if (!focus_key_pressed) return;

    // `switch-focus-ignore-class-forward'
    // `switch-focus-ignore-class-backward'
    if (meta_down)
      set_focus (next_client_normal (focus, !shift_down), true);


    // `switch-focus-different-class-forward'
    // `switch-focus-different-class-backward'
    else if (alt_down)
      set_focus (next_client_different_class (focus, !shift_down), true);


    // `switch-focus-same-class-forward'
    // `switch-focus-same-class-backward'
    else if (super_down)
      set_focus (next_client_same_class (focus, !shift_down), true);
  }


  public void key_warp_pointer () {
    if (!argument_present) {
      focus.warp_pointer (10, 10);
      return;
    }
      
    Point position = pref.warp_position (argument);
    if (position == null)
      alert_user ("Warp position not defined: " + argument);
    else
      root.warp_pointer (position);
  }


  public void manage (Client client) {
    if (!clients.contains (client)) clients.add (client);

    // ready for move and resize
    client.get_geometry ();

    // ready for next focus and preference
    client.class_hint = client.wm_class_hint ();   

    // ready for minimize
    client.size_hints = client.wm_normal_hints ();

    // ready for info
    client.name = client.wm_name ();


    client.change_save_set (false);


    if (!grant_preference (client))
      // no position preference, do bounding box
      client.move_resize (client.rectangle ().within (space));
  }


  public void maximize (Client client, boolean full_screen) {
    if (full_screen)
      client.move_resize (0, 0, display.default_screen.width, 
	display.default_screen.height);

    else
      client.move_resize (space);
  }


  public void minimize (Client client) {
    client.width = client.size_hints == null || 
      client.size_hints.min_width () == 0 ?
      client.width/2 : client.size_hints.min_width ();

    client.height = client.size_hints == null || 
      client.size_hints.min_height () == 0 ?
      client.height/2 : client.size_hints.min_height ();

    client.resize ();
    relocate (client);
  }


  public Client next_client (Client from, boolean reverse) {
    int index = clients.indexOf (from);
    int n = clients.size ();

    // + n for no negative value
    // % n for cycle
    index = (reverse ? index-1+n : index+1+n) % n;

    Client next = (Client) clients.elementAt (index);

    /* Detect looping.
     *
     * Case !focus_key_pressed: focus is always at the top of
     * clients, i.e., the end of the vector. next == focus means we
     * are searching in a loop. We return null to signal it. Callers should
     * quit recursive searching, and return null to set_focus, which does
     * nothing (as search fails).
     *
     * Case focus_key_pressed: focus_base is the last active window. 
     * Note orders of windows in clients stays constants during
     * focus_key_pressed, and thus focus cannot be used to detect
     * looping. next == focus_base means we are searching in a loop. 
     * We return null to signal it. Callers should quit recursive
     * searching, and return null to set_focus, which, however, sets focus
     * to focus_base, to allow circular tabbing.
     *
     * Note that returning null (instead of focus_base) is critical in
     * next_* () methods as, say, next_client_normal () will be called by,
     * say, next_client_first_char (), returning null in next_client_normal
     * () prevents looping in next_client_first_char ().
     */
    if (focus_key_pressed && next == focus_base) return null;
    if (!focus_key_pressed && next == focus) return null;

    if (next.early_unmapped || next.early_destroyed) 
      return next_client (next, reverse);
    else
      return next;
  }


  /**
   * @see #next_client(Client, boolean)
   */
  public Client next_client_normal (Client from, boolean reverse) { 
    Client next = next_client (from, reverse);
    if (next == null) return null; // prevent looping
    if (next.state == NORMAL) return next;
    return next_client_normal (next, reverse); // recursive
  }


  /**
   * @see #next_client(Client, boolean)
   */
  public Client next_client_same_class (Client from, boolean reverse) {
    Client next = next_client_normal (from, reverse);
    if (next == null) return null; // prevent looping

    if (focus_base.class_hint != null
      && next.class_hint != null
      && focus_base.class_hint.class_equals (next.class_hint))

      return next;

    return next_client_same_class (next, reverse); // recursive
  }


  /**
   * @see #next_client(Client, boolean)
   */
  public Client next_client_different_class (Client from, 
    boolean reverse) { 
    
    Client next = next_client_normal (from, reverse);
    if (next == null) return null; // prevent looping

    /* Check if WM-CLASS of next window is different from all we have
     * processed so far since first pressing SUPER-TAB.
     *
     * Respect null res_class.
     */
    String res_class = "";
    if (next.class_hint != null) res_class = next.class_hint.res_class ();
    if (!focus_so_far.contains (res_class)) {
      focus_so_far.add (res_class);
      return next;
    }

    return next_client_different_class (next, reverse);	// recursive
  }



  /**
   * @see #next_client(Client, boolean)
   */
  public Client next_client_first_char (Client from, boolean reverse,
    char c, boolean all) {

    Client next = next_client (from, reverse);
    if (next == null) return null; // prevent looping

    boolean state = all ? 
      // no UNMAPPED
      next.state == NORMAL | next.state == HIDDEN | next.state == NO_FOCUS
      : next.state == NORMAL;

    if (next.class_hint != null) {
      String c1 = next.class_hint.res.substring (0, 1);
      String c2 = String.valueOf (c);

      if (state && next.class_hint.res.length () > 0
        && c1.equalsIgnoreCase (c2))

        return next;
    }

    return next_client_first_char (next, reverse, c, all); // recursive
  }


  public boolean system_key_pressed, focus_key_pressed;

  
  public void when (Event event) {
    if (print_event) System.out.println (event);

    switch (event.code ()) {
    case ButtonPress.CODE:
      when_button_press ((ButtonPress) event);
      break;

    case ClientMessage.CODE: // un-avoidable
      when_client_message ((ClientMessage) event);
      break;

    case ConfigureRequest.CODE: // Event.SUBSTRUCTURE_NOTIFY
      when_configure_request ((ConfigureRequest) event);
      break;

    case DestroyNotify.CODE: // Event.SUBSTRUCTURE_NOTIFY
      when_destroy_notify ((DestroyNotify) event);
      break;

    case KeyPress.CODE:	// grab key
      when_key_press ((KeyPress) event);
      break;

    case KeyRelease.CODE:	// grab key
      when_key_release ((KeyRelease) event);
      break;

    case PropertyNotify.CODE: // Event.PROPERTY_CHANGE
      when_property_notify ((PropertyNotify) event);
      break;

    case MapRequest.CODE:	// Event.SUBSTRUCTURE_REDIRECT
      when_map_request ((MapRequest) event);
      break;

    case MapNotify.CODE:	// Event.SUBSTRUCTURE_NOTIFY
      when_map_notify ((MapNotify) event);
      break;
      
    case UnmapNotify.CODE:	 // Event.SUBSTRUCTURE_NOTIFY
      when_unmap_notify ((UnmapNotify) event);
      break;

    case ConfigureNotify.CODE: // Event.SUBSTRUCTURE_NOTIFY, ignored
    case CreateNotify.CODE: // Event.SUBSTRUCTURE_NOTIFY, ignored
    case MappingNotify.CODE: // un-avoidable, ignored TODO
      break;

    default:
      alert_user ("Unhandled event: " + event);
    }
  }


  public void read_and_dispatch_event () {
    Event first_event = display.next_event ();
   
    /* Race conditions: while reading an event of a window, the window may
     * have already unmapped or even destroyed. UnmapNotify or
     * DestroyNotify of the window may come later in the event queue,
     * or they are just being generated in the server and not yet reach our
     * queue.
     *
     * Solution: We MUST grab the server to halt further event generations
     * during processing events. Before acting on any event with {@link
     * #when(Event)}, we pull all events in the network and check for
     * <code>UnmapNotify</code> and <code>DestroyNotify</code>. 
     * Note we force X server to flush all pending events for us using the
     * technique described {@link Connection#pull_all_events() here}. We then
     * set {@link Client#early_unmapped} and {@link Client#early_destroyed}
     * accordingly. Event handling routines may check these client flags
     * and decide if they should abort the operations, as the window may
     * have already unmapped/destroyed.
     *
     * For example, {@link #when_key_press(KeyPress)} depends on the
     * visibility of a window and thus aborts the operations if the window
     * is already unmapped or destroyed. On the other hand, {@link
     * #when_property_notify(PropertyNotify)} depends only on the
     * liveness of a window and thus aborts the operations only when the
     * window is already destroyed.
     *
     * To street test the correctness of this method (or that of other
     * window manager), drag around any icon in Netscape so that Netscape
     * will quickly map, unmap, and destroy windows. For instance, remove
     * <code>display.check_error ();</code> here and run the test, Puppet
     * will produce an {@link Error#BAD_WINDOW} error.
     */
    display.grab_server ();
    
    display.check_error ();
    List other_events = display.in.pull_all_events ();
    
    for (Iterator it=other_events.iterator (); it.hasNext ();) {       
      Event event = (Event) it.next ();

      if (event.code () == DestroyNotify.CODE) {	
        Client client = (Client) Client.intern (display, 
          ((DestroyNotify) event).window_id);
        client.early_destroyed = true;

      } else if (event.code () == UnmapNotify.CODE) {
        Client client = (Client) Client.intern (display, 
          ((UnmapNotify) event).window_id);
        client.early_unmapped = true;
      }
    }


    when (first_event);    


    /* Grabbing server significantly hinders responsiveness of this
     * window manager. While we have to trade speed for correctness,
     * we can aggregate grabbing and ungrabbing by processing all events in
     * queue at once between grabbings.
     */
    for (Iterator it=other_events.iterator (); it.hasNext ();)
      when ((Event) it.next ());


    /* We must flush all requests before un-grabbing the server, so that
     * windows in the requests are guaranteed to be alive.
     */
    display.flush ();

    display.ungrab_server ();
  }


  // find another client of same res_name and res_class
  public void register_fall_back (Client client) {
    if (client.register == -1) return;
    Client fall_back = null;

    if (client.class_hint != null)
      for (Iterator it=clients.iterator (); it.hasNext ();) {
        Client c = (Client) it.next ();

        if (c != client           // another!
          && c.class_hint.equals (client.class_hint)
          && !c.early_unmapped && !c.early_destroyed) {
        
          fall_back = c;
          break;
        }
      }

    registers [client.register] = fall_back;
    client.register = -1;
  }


  public static final java.util.Random random = new java.util.Random ();


  public void relocate (Client client) {
    int x_range = space.width - client.width;
    int y_range = space.height - client.height;

    if (x_range < 0) x_range = 0;
    if (y_range < 0) y_range = 0;
   
    client.x = x_range == 0 ? 0 : random.nextInt (x_range);
    client.y = y_range == 0 ? 0 : random.nextInt (y_range);


    // origin of space
    client.x += space.x;
    client.y += space.y;

    client.move ();
  }


  public void restore_size (Client client) {
    client.resize (client.saved_width, client.saved_height);
  }


  public void save_size (Client client) {
    client.saved_width = client.width;
    client.saved_height = client.height;
  }


  public void scale_size (Client client, double factor) {
    client.width *= factor;    
    client.height *= factor;

    client.width = Math.min (client.width, space.width);
    client.height = Math.min (client.height, space.height);

    client.resize ();
    relocate (client);
  }


  public void scan_children () {
    // query all top-level windows
    Window[] children = root.tree ().children ();
    for (Window w : children) {
      Client client = (Client) Client.intern (display, w.id);

      // get override_redirect and map_state
      client.attributes = client.attributes ();

      /* Children of root to be managed: (1) not override redirect (pop-up
       * and transient windows). (2) VIEWABLE (previously mapped)
       */
      if (client.attributes.override_redirect ()
	|| client.attributes.map_state ()
        != Window.AttributesReply.VIEWABLE)

	continue;		// not managed


      Window.WMState wm_state = client.wm_state ();
      if (wm_state != null && wm_state.state () == Window.WMState.ICONIC) {
	hide (client);		// respect its iconic state

      } else {
	// maintain our state variable
	client.state = NORMAL;
	// in case someone screws this up
	client.set_wm_state (Window.WMState.NORMAL);
      }


      if (client.state == NORMAL || client.state == HIDDEN)
	manage (client);
    }
  }


  public void set_focus (Client client, boolean warp_pointer) {
    /* Process return value from next_client_* () to prevent looping.
     *
     * Case 1 = !focus_key_pressed && client == null. next_client_* fails
     * to find a matching client. Operation aborts and this method simply
     * returns.
     *
     * Case 2 = focus_key_pressed && client == null. next_client_* detects
     * a cycle (start pressing TAB on client A, navigate all matching
     * clients, and come back to client A). Cycling operation restarts and
     * this method continues with client = focus_base.
     *
     * @see #next_client(Client, boolean)
     * @see #focus_so_far
     */
    if (!focus_key_pressed && client == null) return;
    
    if (focus_key_pressed && client == null) {
      client = focus_base;
      focus_so_far.clear ();
      if (focus_base.class_hint != null)
        focus_so_far.add (focus_base.class_hint.res_class ());
    }

    client.raise ();		// we choose to do it
    focus = client;

    /* At this point, and only at this point, we are 100% sure that the
     * window is mapped and hence can be assigned focus to. Otherwise, an
     * {@link Error#BAD_WINDOW} will result.
     */
    focus.set_input_focus ();
    
    // give hint
    if (warp_pointer) client.warp_pointer (10, 10);


    /* For changing focus in focus_key_pressed mode, do not update client order
     * until the user decides which window to stay with and releases focus
     * key modifier.
     *
     * @see #when_key_release(KeyRelease)
     */
    if (!focus_key_pressed) update_client_order (focus);
  }


  public void when_button_press (ButtonPress event) {
    /* Because we are doing grabbing on root window, we should not check
     * <code>event.window()</code> since it always equals to
     * {@link #root}. Instead, check <code>event.child()</code>.
     */    
    boolean control_down = (event.state () & Input.CONTROL_MASK) != 0;
    boolean meta_down = (event.state () & Input.META_MASK) != 0;
    boolean on_root = event.child_id () == 0;

    if (meta_down && on_root) { // `lanuch-on-root'
      try {
        Runtime.getRuntime ().exec (pref.launch_on_root ());
      } catch (java.io.IOException e) {
        alert_user ("Failed to launch " + pref.launch_on_root ()
          + ": " + e);
      }
      return;
    }

    
    if (!meta_down && on_root) { // `pointer-root-focus'
      root.set_input_focus ();
      return;
    }
       

    Client client = (Client) Client.intern (display, event.child_id ());
    if (client.early_unmapped || client.early_destroyed) return;
    if (!control_down) return;
    int button = event.detail ();

    switch (button) {
    case Input.BUTTON1:
      if (meta_down)            // `delete-window'
	client.delete ();

      else {                    // `focus-with-raise'
	// same as set_focus ()
	client.raise ();
	focus = client;
	focus.set_input_focus ();
	update_client_order (focus);
      }
      break;

    case Input.BUTTON2:         // `focus-without-raise'
      focus = client;
      focus.set_input_focus ();
      update_client_order (focus);
      break;

    case Input.BUTTON3:
      if (meta_down) client.kill (); // `kill-window'
      
      else {                    // `lower-behind'
	// give focus to some other window (under pointer)
	client.lower ();
	focus = (Client) Client.intern (root.query_pointer ().child);
	focus.set_input_focus ();    
	update_client_order (focus); // != client
      }
      break;
    }
  }


  public void when_client_message (ClientMessage event) {
    // client asks to change window state from normal to iconic 
    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_unmapped || client.early_destroyed) return;

    Atom type = event.type ();
    if (event.format () == 32 
      && type.name.equals ("WM_CHANGE_STATE")
      && event.wm_data () == Window.WMHints.ICONIC) {

      hide (client);

    } else
      alert_user ("Unhandled client message: " + type);
  }


  public void when_configure_request (ConfigureRequest event) {
    // client asks to change window configuration
    // @see icccm/sec-4.html#s-4.1.5

    // TODO find space in screen to display window?
    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_unmapped || client.early_destroyed) return;


    if (client.class_hint != null) {
      String id = client.class_hint.res + ":";
      if (pref.no_geometry_change (id)) return;
    }


    /* Should I send a synthetic ConfigureNotify instead of actually
     * do a configure request on the window? We do not re-parent, and thus,
     * according to icccm, a ConfigureNotify will be fine. But xterm
     * relies on a window manager to honour its ConfigureRequest to
     * configure a window, or it falls back to width = height = 1. A mere
     * ConfigureNotify seems not sufficient. (Other clients does not
     * have this problems?)
     */
    client.configure (event.changes ());
    
    client.set_geometry_cache (event.rectangle ());


    // we choose to give it focus if it is normal and it raises
    if (client.state == NORMAL
      && event.stack_mode () == Window.Changes.ABOVE)

      set_focus (client, false);
  }


  public void when_destroy_notify (DestroyNotify event) {    
    Client client = (Client) Client.intern (display, event.window_id);
    give_up_focus (client);
    
    register_fall_back (client);
    client.unintern ();
    clients.remove (client);
  }


  /* The following variables are global initialized in
   * #when_key_press. They are supposed to be global to all
   * key_* methods such that these critical information are available to
   * those methods without passing lots of variables.
   */
  public int keycode, keysym;
  public boolean alt_down, control_down, meta_down, shift_down, super_down;
  public int prefix0, prefix1;  
  public int argument;
  public boolean argument_present;
  public boolean argument_negative;


  public void when_key_press (KeyPress event) {
    keycode = event.detail ();
    int keystate = event.state ();
    keysym = display.input.keycode_to_keysym (keycode, keystate);

    shift_down = (keystate & Input.SHIFT_MASK) != 0;
    control_down = (keystate & Input.CONTROL_MASK) != 0;
    meta_down = (keystate & Input.META_MASK) != 0;    
    alt_down = (keystate & Input.ALT_MASK) != 0;    
    super_down = (keystate & Input.SUPER_MASK) != 0;    


    if (!system_key_pressed && !focus_key_pressed) {
      int status = root.grab_keyboard (false, Window.ASYNCHRONOUS,
        Window.ASYNCHRONOUS, display.CURRENT_TIME); 
      if (status != Window.SUCCESS)
	throw new RuntimeException ("Failed to grab keyboard");

      focus_key_pressed = keysym == Misc.TAB;
      system_key_pressed = !focus_key_pressed;
      if (system_key_pressed) return;

      // if tab (see #next_client)
      focus_base = focus;
      focus_so_far.clear ();
      if (focus_base.class_hint != null)
        focus_so_far.add (focus.class_hint.res_class ());
    }


    if (focus_key_pressed) {
      if (keysym == Misc.ESCAPE) { // abort-switch-focus
        focus_key_pressed = false;
        set_focus (focus_base, true);

      } else if (keysym ==  Misc.TAB)
        key_switch_focus ();
   
      return;
    }


    if (key_process ()) {       // done
      display.input.ungrab_keyboard ();
      system_key_pressed = false;
      prefix0 = prefix1 = 0;

      argument = 0;
      argument_negative = false;
      argument_present = false;
    }
  }


  /** Handle *-TAB key release. */
  public void when_key_release (KeyRelease event) {
    keycode = event.detail ();
    int keystate = event.state ();
    keysym = display.input.keycode_to_keysym (keycode, keystate);

    if (!focus_key_pressed
      || !(keysym == Misc.META_L
	|| keysym == Misc.META_R
	|| keysym == Misc.ALT_L
	|| keysym == Misc.ALT_R
	|| keysym == Misc.SUPER_L
	|| keysym == Misc.SUPER_R)) return;

    display.input.ungrab_keyboard ();
    focus_key_pressed = false;

    // finally user has chosen a new active window
    update_client_order (focus);
  }

  
  public void when_property_notify (PropertyNotify event) {
    Atom atom = event.atom (display);

    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_destroyed) return;

    if (atom == wm_colormap_windows
      || atom == wm_protocols)
      throw new java.lang.Error ("unhandled property notfiy: " + atom);

    switch (atom.id) {  
    case Atom.WM_HINTS_ID:	// TODO any action?
      client.wm_hints ();
      break;

    case Atom.WM_NORMAL_HINTS_ID: // TODO any action?
      client.size_hints = client.wm_normal_hints ();
      break;

    case Atom.WM_NAME_ID:	
      client.name = client.wm_name ();
      break;

    case Atom.WM_ICON_NAME_ID:	// fall through
    case Atom.WM_TRANSIENT_FOR_ID:
      // ignore (normal window manager should handle these)
      break;
    }
  }


  public void when_map_request (MapRequest event) {
    // client asks to change window state from withdrawn to normal/iconic,
    // or from iconic to normal
    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_unmapped || client.early_destroyed) return;


    // get override_redirect and map_state
    client.attributes = client.attributes ();
    if (client.attributes.override_redirect ()) return;

    manage (client);

    Window.WMHints wm_hints = client.wm_hints ();

    // assume NORMAL if initial_state not specified
    if (wm_hints == null
      || (wm_hints.flags () & Window.WMHints.STATE_HINT_MASK) == 0
      || wm_hints.initial_state () == Window.WMHints.NORMAL) {

      /* Do not do any visible operations on the window such as focusing
       * and warping pointer, until a window is actually map, ie. 
       * MapNotify.
       *
       * @see #when_map_notify(MapNotify)
       */
      client.map ();
      
    } else {			// must be iconic
      client.state = HIDDEN;
      client.set_wm_state (Window.WMState.ICONIC);
    }
  }


  public void when_map_notify (MapNotify event) {
    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_unmapped || client.early_destroyed) return;


    /* Get override_redirect and map_state.
     *
     * Case 1. !override_redirect (). MapRequest is generated and
     * client.attributes is already initialized in when (MapRequest)
     *
     * Case 2. override_redirect (). NO MapRequest is generated and
     * client.attributes == null - it must be initialized here.
     *
     * @see <a href="XMapWindow.html">XMapWindow</a>
     */
    if (client.attributes == null)
      client.attributes = client.attributes ();

    if (client.attributes.override_redirect ()) return;

    /* Now and only now sets the window state to NORMAL (except during
     * initialization). Setting this earlier gives false impression that
     * the window is mapped, but it does not happen until MapNotify.
     * Note window.raise () and window.map () do not guarantee the
     * visibility of a window (due to map request and configure request
     * redirection of wm). Hence, any operations that depends on visibility
     * (warp pointer and set input focus) should check window.state.
     *
     * state == NO_FOCUS from grant_preference ().
     */
    if (client.state != NO_FOCUS) client.state = NORMAL;
    client.set_wm_state (Window.WMState.NORMAL);

    /* We can set focus to a window only when it is ready, ie.
     * MapNotify, not MapRequest.
     */
    if (client.state != NO_FOCUS) set_focus (client, false);
  }


  public void when_quit () {
    // so that I can keep typing
    focus.warp_pointer (10, 10);

    // set_input_focus () to POINTER_ROOT to restore the default focus
    // (every wm should do this)?
    Window.POINTER_ROOT.display = display;
    Window.POINTER_ROOT.set_input_focus ();
  }
 

  public void when_unmap_notify (UnmapNotify event) {
    /* Unmapped != unmanaged, since it can be iconify-ing (or hiding in our
     * case). We unmanage a window when it is destroyed.
     *
     * @see #when_destroy_notify(DestroyNotify)
     */
    Client client = (Client) Client.intern (display, event.window_id);
    if (client.early_destroyed) return;

    client.early_unmapped = false; // handled here
    give_up_focus (client);

    // they withdraw it
    if (client.state != HIDDEN) {
      /* From icccm 4.1.4: For compatibility with obsolete clients, window
       * managers should trigger the transition to the Withdrawn state on
       * the real UnmapNotify rather than waiting for the synthetic one. 
       * They should also trigger the transition if they receive a
       * synthetic UnmapNotify on a window for which they have not yet
       * received a real UnmapNotify.
       *
       * Then, what's the use of synthetic UnmapNotify event?
       */
      client.state = UNMANAGED;
      client.set_wm_state (Window.WMState.WITHDRAWN);
      client.change_save_set (true);
    }
  }


  public void toggle_focus (Client client) {
    if (client.state == NORMAL)
      deny_focus (client);
    else if (client.state == NO_FOCUS)
      grant_focus (client);
  }


  public void unhide (Client client) {
    if (client.state != HIDDEN) return;

    /* Do not set client.state here. Do it right in
     * {@link #when_map_notify(MapNotify)}.
     */
    client.map ();
  }


  public void unhide_all () {
    for (Iterator it=clients.iterator (); it.hasNext ();) {
      unhide ((Client) it.next ());
    }
  }


  public void unhide_same_class (Client client) {
    for (Iterator it=clients.iterator (); it.hasNext ();) {
      Client c = (Client) it.next ();

      if (c.state == HIDDEN
        && c.class_hint != null
	&& c.class_hint.class_equals (client.class_hint))

	unhide (c);
    }
  }


  public void update_client_order (Client client) {
    clients.remove (client);
    clients.add (client);
  }


  public static void main (String [] args) throws NotFoundException {
    new Puppet (args);
  }
}
