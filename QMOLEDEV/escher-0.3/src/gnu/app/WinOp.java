package gnu.app;

import gnu.x11.Window;


/**
 * Perform action on window given its ID. This can be useful for debugging
 * window programming.
 * 
 * @see <a href="../../../etc/screenshot/gnu/app/WinOp.help">
 * help output</a>
 */
public class WinOp extends gnu.x11.Application {
  public static final int LIST_TOP_LEVEL = 0;
  public static final int LIST_CHILDREN = 1;
  public static final int MAP = 2;
  public static final int SET_INPUT_FOCUS = 3;


  public static final String [] OPERATION_STRINGS = {
    "list-top-level",
    "list-children",
    "map",
    "set-input-focus"
  };


  Window window;


  public WinOp (String [] args) {
    super (args);

    int window_id = option.intt ("window-id",
      "window id (0 = default root)", 0);
    int operation = option.enumerate ("operation",
      "action on window", OPERATION_STRINGS, LIST_TOP_LEVEL);
    
    about ("0.1", "perform action on window given its ID",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    
    if (window_id == 0) window_id = display.default_root.id;
    window = (Window) Window.intern (display, window_id);
    
    switch (operation) {
    case LIST_TOP_LEVEL:
      list_top_level ();
      break;

    case LIST_CHILDREN:
      list_children ();
      break;
      
    case MAP:
      window.map ();
      break;

    case SET_INPUT_FOCUS:
      window.set_input_focus ();
      break;
    }

    display.close ();
  }


  public void list_top_level () {
    // query all top-level windows
    for (Window child : display.default_root.tree ().children ()) {

      // selection criteria
      Window.AttributesReply atts = child.attributes ();
      String name = child.wm_name ();
      if (! atts.override_redirect ()
          && atts.map_state () == Window.AttributesReply.VIEWABLE
	  && name != null) {

        System.out.println (child.id + " " + name);

      }
    }
  }

  
  public void list_children () {
    System.out.println ("---- children of " + window);
    Window [] children = window.tree ().children ();
    for (Window child : children) {
      System.out.println(child.toString ());
    }
  }


  public static void main (String args []) {
    new WinOp (args);
  }
}
