package gnu.x11.test;


/**
 * Test ZPixmap pixel manipulation. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/ZPixmap.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/ZPixmap.help">
 * help output</a>
 */
public class ZPixmap extends Graphics {
  gnu.x11.image.ZPixmap zpixmap;


  public ZPixmap (String [] args) { 
    super (args, 256, 256); 

    about ("0.1", "test zpixmap pixel manipulation",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    int len = 256;
    zpixmap = new gnu.x11.image.ZPixmap (display, len, len,
                                         display.default_pixmap_format);

    // straight from XTC
    for (int y=0; y<len; y++) {
      for (int x=0; x<len; x++) {
        int r = (x*y) & 0xff;
        int g = x^y;
        int b = (x*y>>1) & 0xff;
        zpixmap.set (x, y, r, g, b);
      }
    }
  }
   

  public void paint () {
    window.put_image (display.default_gc, zpixmap, 0, 0);
  }


  public static void main (String [] args) {
    new ZPixmap (args).exec ();
  }
}
