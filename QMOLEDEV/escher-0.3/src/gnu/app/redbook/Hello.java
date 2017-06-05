package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Hello World. Modified from <code>hello.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Hello.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/Hello.help">
 * help output</a>
 */
public class Hello extends gnu.x11.extension.glx.Application {
  public Hello (String [] args) {
    super (args, 0);

    about ("0.1", "hello world",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (250, 250);

    // initialize viewing values
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    gl.ortho (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
  }


  protected void handle_expose () {
    // clear all pixels
    gl.clear (GL.COLOR_BUFFER_BIT);

    // white polygon (rectangle)
    gl.begin (GL.POLYGON);
    gl.vertex2f (-0.5f, -0.5f);
    gl.vertex2f (-0.5f, 0.5f);
    gl.vertex2f (0.5f, 0.5f);
    gl.vertex2f (0.5f, -0.5f);
    gl.end ();

    // action!
    gl.swap_buffers (window);
  }


  public static void main (String [] args) {
    new Hello (args).exec ();
  }
}
