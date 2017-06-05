package gnu.x11.test;

import gnu.x11.GC;
import gnu.x11.Pixmap;
import gnu.x11.extension.render.DrawablePicture;
import gnu.x11.extension.render.PictFormat;
import gnu.x11.extension.render.Render;
import gnu.x11.extension.render.Picture;


/**
 * Test blending in X Rendering Extension.
 *
 * <p>Modified from <code>blend.c</code> in <a
 * href="http://xfree86.org/~keithp/download/test.tar.bz2">render sample
 * code</a> by Keith Packard.
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Blend.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Blend.help">
 * help output</a>
 */
public class Blend extends Graphics {
  public GC alpha_gc, color_gc;
  public Picture alpha_picture, color_picture, window_picture;
  public Pixmap alpha_pixmap, color_pixmap;
  public Render render;


  public Blend (String [] args) throws gnu.x11.extension.NotFoundException {
    super (args, 255, 255);

    about ("0.1", "test blending in RENDER",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    render = new Render (display);
    PictFormat pf1;

    alpha_pixmap = new Pixmap (window, window.width, window.height, 8);
    color_pixmap = new Pixmap (window, 1, 1, 24);
    alpha_gc = new GC (alpha_pixmap);
    color_gc = new GC (color_pixmap);


    // window picture (TODO: find visual)
    PictFormat.Template pf0 = new PictFormat.Template ();
    pf0.set_depth (display.default_screen.root_depth ());
    pf1 = render.picture_format (pf0, true);

    window_picture = render.create_picture (window, pf1, 
      DrawablePicture.Attributes.EMPTY);


    // alpha picture
    pf0.clear ();
    pf0.set_depth (8);
    pf0.set_type (PictFormat.Type.DIRECT);
    pf0.set_direct(0, 0, 0, 0, 0, 0, 0, 0xff);
    pf1 = render.picture_format (pf0, true);

    alpha_picture = render.create_picture (alpha_pixmap, pf1,
      DrawablePicture.Attributes.EMPTY);


    // color picture
    pf0.clear ();
    pf0.set_depth (24);
    pf0.set_type (PictFormat.Type.DIRECT);
    pf0.set_direct (16, 0xff, 8, 0xff, 0, 0xff, 0, 0);
    pf1 = render.picture_format (pf0, true);

    DrawablePicture.Attributes attr = new DrawablePicture.Attributes ();
    attr.set_repeat (true);
    color_picture = render.create_picture (color_pixmap, pf1, attr);
  }


  public void paint () {
    window.clear (false);

    color_gc.set_foreground (0xff0000);
    color_pixmap.rectangle (color_gc, 0, 0, 1, 1, true);

    alpha_gc.set_foreground (0x00);
    alpha_pixmap.rectangle (alpha_gc, 0, 0, 
      window.width, window.height, true);
    alpha_gc.set_foreground (0x7f);
    alpha_pixmap.rectangle (alpha_gc, 10, 10, 50, 50, true);

    render.composite (Render.OVER, color_picture, alpha_picture,
      window_picture, 0, 0, 0, 0, 0, 0, window.width, window.height);

    color_gc.set_foreground (0x0000ff);
    color_pixmap.rectangle (color_gc, 0, 0, 1, 1, true);

    alpha_gc.set_foreground (0x00);
    alpha_pixmap.rectangle (alpha_gc, 0, 0, 
      window.width, window.height, true);
    alpha_gc.set_foreground (0x7f);
    alpha_pixmap.rectangle (alpha_gc, 40, 40, 50, 50, true);

    render.composite (Render.OVER, color_picture, alpha_picture,
      window_picture, 0, 0, 0, 0, 0, 0, window.width, window.height+10);
    display.flush ();
  }


  public static void main (String [] args) throws Exception {
    new Blend (args).exec ();
  }
}
