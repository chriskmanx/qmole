package gnu.x11.test;

import gnu.util.Misc;
import gnu.x11.Atom;
import gnu.x11.Font;
import gnu.x11.Window;
import gnu.x11.extension.NotFoundException;
import gnu.x11.extension.render.Render;
import gnu.x11.extension.glx.GLX;
import gnu.x11.extension.glx.GL;


/** 
 * List info about X server. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Info.output">
 * text output on linux + xfree86 4.0</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Info.output.solaris">
 * text output on solaris + x11</a>
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Info.output.windows">
 * text output on window 98 + winpro</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Chinese.help">
 * help output</a>
 */
public class Info extends gnu.x11.Application {
  public Info (String [] args) { 
    super (args);

    boolean print_keysyms = option.booleann ("print-keysyms",
      "print all keysyms for debug", false);

    about ("0.1", "list info about X server",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    System.out.println ("\n\n---- display\n" + display);

    System.out.println ("\n\n---- extension");
    String[] exts = display.extensions ();
    for (int i = 0; i < exts.length; i++)
      System.out.println(exts[i]);

    extension_details ();

    System.out.println ("\n\n---- keyboard control\n"
      + display.input.keyboard_control ());
    
    System.out.println ("\n\n---- pointer control\n"
      + display.input.pointer_control ());
    
    System.out.println ("\n\n---- screen saver\n"
      + display.screen_saver ());
    
    System.out.println ("\n\n---- font path");
    String[] fontpath = display.font_path ();
    for (int i = 0; i < fontpath.length; i++)
      System.out.println(fontpath[i]);

    System.out.println ("\n\n---- first 20 fonts");
    Font[] fonts = display.fonts ("*", 20);
    for (int i = 0; i < fonts.length; i++)
      System.out.println(fonts[i]);

    System.out.println ("\n\n---- children of root");
    Window[] children = display.default_root.tree ().children ();
    for (int i = 0; i < children.length; i++)
      System.out.println(children[i]);

    System.out.println ("\n\n---- properties of root");
    Atom[] props = display.default_root.properties ();
    for (int i = 0; i < props.length; i++)
      System.out.println(props[i]);

    System.out.println ("\n\n---- screens"
      + Misc.to_string (display.screens));

    System.out.println ("\n\n---- pixmap formats"
      + Misc.to_string (display.pixmap_formats));


    System.out.println ("\n\n---- keyboard symbols");
    System.out.println ("  min-keycode: " + display.input.min_keycode);
    System.out.println ("  max-keycode: " + display.input.max_keycode);
    System.out.println ("  keycode-count: "
      + (display.input.max_keycode - display.input.min_keycode + 1));
    System.out.println ("  keysyms-per-keycode: : " 
      + display.input.keysyms_per_keycode);

    // compare to "xmodmap -pk"
    if (print_keysyms) {
      System.out.println ("  ** keysyms **");
      for (int i=0; i<display.input.keysyms.length; i++)
        System.out.println (i+display.input.min_keycode + ": " 
          + display.input.keysyms [i]);
    }
  }


  public void extension_details () {
    System.out.println ("\n\n---- extension details");

    try {
      System.out.println (new gnu.x11.extension.
      BigRequests (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("big requests not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.DBE (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("dbe not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.DPMS (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("dpms not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.EVI (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("evi not found\n");
    }

    try {
      GLX glx = new GLX (display);
      System.out.println (glx + Misc.to_string (glx.visual_configs (0)));

      GL gl = glx.create_context (display.default_screen.root_visual_id (),
        display.default_screen_no, GL.NONE0);
      gl.make_current (display.default_root);
      System.out.println (gl + "\n");

    } catch (NotFoundException e) {
      System.out.println ("glx not found\n");
    }

    try {
      Render render = new Render (display);
      System.out.println (render 
        + Misc.to_string (render.picture_formats ()));
    } catch (NotFoundException e) {
      System.out.println ("render not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.Shape (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("shape not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.XCMisc (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("xcmic not found\n");
    }

    try {
      System.out.println (new gnu.x11.extension.XTest (display) + "\n");
    } catch (NotFoundException e) {
      System.out.println ("xtest not found\n");
    }
  }


  public static void main (String [] args) { 
    new Info (args);
  }
}
