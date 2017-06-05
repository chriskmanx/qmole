package gnu.app.puppet;

import gnu.x11.Point;
import gnu.x11.Rectangle;
import java.util.Hashtable;


/**
 * User settings of {@link Puppet}.
 * 
 * You can find out WM-NAME and WM-CLASS of any window using standard X
 * Window System utilities such as `xwininfo' and `xprop', or you can use
 * `key-dump-info' of Puppet.
 */
public class Preference {
  /* My desktop.
   *
   * licq 0.85
   * pclock -w -B $HOME/lib/ugly-yellow.xpm -H blue
   * wmload -exe puppet
   * xeyes
   * xsetbg $HOME/lib/echer.jpg
   */

  private static final String [] [] NO_FOCUS = {
    {"MainWindow", "licq"},     // valid only for licq 0.85
    {"pclock", "PClock"},
    {"wmload", "WMLoad"},
    {"xeyes", "XEyes"}
  };


  private static final String [] [] NO_GEOMETRY_CHANGE = {
    {"MainWindow", "licq"},
  };


  private static final Object [] [] POSITION = {
    {"MainWindow", "licq", new Rectangle (0, 0, 135, 1024)},
    {"findDialog_popup", "Netscape", new Rectangle (860, 30, 0, 0)},
    {"pclock", "PClock", new Rectangle (-64, 0, 0, 0)},
    {"wmload", "WMLoad", new Rectangle (140, 2, 0, 0)},
    {"gv", "GV", new Rectangle (0, 0, 1400, 1024)},
    {"xeyes", "XEyes", new Rectangle (210, 2, 64, 64)}
  };


  private static final String [] [] REGISTER = { 
    {"MainWindow", "licq"},     // 0
    {"Navigator", "Netscape"},	// 1
    {"emacs", "Emacs"},		// 2
    {"rxvt", "XTerm"},		// 3
    null,			// 4
    null,			// 5
    null,			// 6
    null,			// 7
    null,			// 8
    null                        // 9
  };


  private static final String [] LAUNCH = {
    "licq",			// 0
    "netscape",			// 1
    "emacs",			// 2
    "rxvt",			// 3
    null,                       // 4
    null,                       // 5
    null,                       // 6
    null,                       // 7
    null,                       // 8
    "xterm"                     // 9
  };


  private static final String LAUNCH_ON_ROOT = "rxvt";


  private static final Point [] WARP = {
    new Point (60, 1015),       // 0 - licq message
    null,                       // 1
    null,                       // 2
    null,                       // 3
    null,                       // 4
    null,                       // 5
    null,                       // 6
    null,                       // 7
    null,                       // 8
    null                        // 9
  };


  private static final Rectangle SPACE
    // small margins (5 pixels) on all sides
    = new Rectangle (140, 70, 1135, 950);


  // end of user setting

  
  private static final Hashtable hash = new Hashtable (29);  
  static {
    // no focus
    for (int i=0; i<NO_FOCUS.length; i++) {
      String key = NO_FOCUS [i] [0] + "\0" + NO_FOCUS [i] [1]
        + ":" + "no-focus";
      hash.put (key, Boolean.TRUE);
    }


    // no geometry change
    for (int i=0; i<NO_GEOMETRY_CHANGE.length; i++) {
      String key = NO_GEOMETRY_CHANGE [i] [0] + "\0" + NO_GEOMETRY_CHANGE [i] [1]
        + ":" + "no-geometry-change";
      hash.put (key, Boolean.TRUE);
    }

    
    // position
    for (int i=0; i<POSITION.length; i++) {
      String key = POSITION [i] [0] + "\0" + POSITION [i] [1]
        + ":" + "position";
      hash.put (key, POSITION [i] [2]);
    }


    // register
    for (int i=0; i<REGISTER.length; i++) {
      if (REGISTER [i] == null) continue;

      String key = REGISTER [i] [0] + "\0" + REGISTER [i] [1]
        + ":" + "register";
      hash.put (key, new Integer (i));
    }
  }


  public boolean no_focus (String id) {
    Object value = hash.get (id + "no-focus");
    return value == Boolean.TRUE;
  }


  public boolean no_geometry_change (String id) {
    Object value = hash.get (id + "no-geometry-change");
    return value == Boolean.TRUE;
  }


  public String launch (int i) { return LAUNCH [i]; }
  public String launch_on_root () { return LAUNCH_ON_ROOT; }


  public Rectangle position (String id) {
    return (Rectangle) hash.get (id + "position");
  }


  public int register (String id) {
    Object value = hash.get (id + "register");

    if (value instanceof Integer) 
      return ((Integer) value).intValue ();
    else
      return -1;
  }


  public Rectangle space () { return SPACE; }
  public Point warp_position (int i) { return WARP [i]; }
}
