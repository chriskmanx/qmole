package gnu.x11.test;

import gnu.x11.image.PPM;
import gnu.x11.image.XBM;
import gnu.x11.image.XPM;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;


/** 
 * Test image output of XBM, XPM, and PPM format. 
 *
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Image.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/x11/test/Image.help">
 * help output</a>
 */
public class Image extends Graphics {
  public static final int [] xbm_data = {
    0xff, 0x00, 0x00, 0xc0, 0xfe, 0x01, 0x00, 0xc0, 0xfc, 0x03, 0x00, 0x60,
    0xf8, 0x07, 0x00, 0x30, 0xf8, 0x07, 0x00, 0x18, 0xf0, 0x0f, 0x00, 0x0c,
    0xe0, 0x1f, 0x00, 0x06, 0xc0, 0x3f, 0x00, 0x06, 0xc0, 0x3f, 0x00, 0x03,
    0x80, 0x7f, 0x80, 0x01, 0x00, 0xff, 0xc0, 0x00, 0x00, 0xfe, 0x61, 0x00,
    0x00, 0xfe, 0x31, 0x00, 0x00, 0xfc, 0x33, 0x00, 0x00, 0xf8, 0x1b, 0x00,
    0x00, 0xf0, 0x0d, 0x00, 0x00, 0xf0, 0x0e, 0x00, 0x00, 0x60, 0x1f, 0x00,
    0x00, 0xb0, 0x3f, 0x00, 0x00, 0x98, 0x7f, 0x00, 0x00, 0x98, 0x7f, 0x00,
    0x00, 0x0c, 0xff, 0x00, 0x00, 0x06, 0xfe, 0x01, 0x00, 0x03, 0xfc, 0x03,
    0x80, 0x01, 0xfc, 0x03, 0xc0, 0x00, 0xf8, 0x07, 0xc0, 0x00, 0xf0, 0x0f,
    0x60, 0x00, 0xe0, 0x1f, 0x30, 0x00, 0xe0, 0x1f, 0x18, 0x00, 0xc0, 0x3f,
    0x0c, 0x00, 0x80, 0x7f, 0x06, 0x00, 0x00, 0xff
  };



  public static final String [] xpm_data = {
    /* width height num_colors chars_per_pixel */
    "    46    44       5            1",
    /* colors */
    ". c white",
    "# c black",
    "a c #00ffff",
    "f c #185d65",
    "g c #2c7d8a",
    /* pixels */
    "##############################################",
    "#############f##f#f###f#f#f####f#f############",
    "###f####f#f###ff##f#f#f#####f#ff#f##f#f####f##",
    "#####f#f###f#f#f#f##f##f##f#ff##f#f#####f#f##f",
    "#f#f#####f#f##f#fff#fff#ff#f##f#f##f##f#####f#",
    "ff##fff#ff#f##f#f#f#ff#f##f#ff#f#ff#ff#f##f#ff",
    "ff#f##f#ff#fffffff#fff#ff#f#f#f#ff#f#f#f##f#ff",
    "##f#ffff#f#f##f#ffff#fffffff##f#ff#f##ffff#fff",
    "fffff#ff#ff#ffffffffffff#f#fff#ff#fffff#fff#f#",
    "f#f#ff#ff#.....ff#ffff#fffffff.......fff##ff#f",
    "fff#ffffffff....ffffffffffffffff...ffff#ffffff",
    "f#fffffffffff....ffffffffffffffff.ffffffff#fff",
    "fffffffffffff.....fffffffffffffff.ffffffffffff",
    "fffffffffffff......ffffffffffffff.ffffffffffff",
    "fffffffffffff.ff....fffffffffffff.ffffffffffff",
    "fffffffffffff.fff....ffffffffffff.ffffffffffff",
    "fffffffffffff.ffff....fffffffffff.ffffffffffff",
    "fffffffffffff.fffff....ffffffffff.ffffffffffff",
    "ffffffffgfaff.faffff....gffgfgfff.ffafafffffff",
    "ffffffaffgfff.ffgfaff....affafaff.gffgffgfffff",
    "fgfafffafafff.fgffgfaf....fgfgffg.fafafgfffaff",
    "gfafaffaffgff.gafafaffa....afffag.gffffafaffff",
    "fgffafaffafaf.gaffafafaf....gfafg.ffgfgfagafaf",
    "gffafffaffgff.gfagfgffgff....gfga.gaffafffffff",
    "ffafgfgfaafaf.gfgfafagagfa....aff.gfggfgaafaff",
    "afgaffgafgfaf.fafggfaagagaf....ag.agaaggagfaff",
    "gfafgafgafagf.agaafagafgagff....a.gfagafggafgf",
    "fgfagfagfgaff.faggagaagagagaf.....aggfagaafagg",
    "gagafagagfgaf.f.............ff....gaaagfaggaff",
    "aagag.f...###.######################....f.agag",
    "gf.f..######...###########################...f",
    ".f.########.....############################..",
    ".########.........############################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
    "##############################################",
  };


  public PPM ppm;
  public XBM xbm;
  public XPM xpm; 


  public Image (String [] args) 
    throws FileNotFoundException, IOException {

    super (args, 200, 80); 

    about ("0.1", "test image output of XBM, XPM, and PPM format",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    System.out.print ("Loading images....");
    xbm = new XBM (display, 32, 32, xbm_data);
    xpm = new XPM (display, xpm_data);
    FileInputStream ppm_file = new FileInputStream ("etc/image.pnm");
    ppm = new PPM (display, ppm_file);
    System.out.println ("done.");
  }


  public void paint () {    
    window.put_image (display.default_gc, xbm, 20, 25);
    window.put_image (display.default_gc, xpm, 80, 20);
    window.put_image (display.default_gc, ppm, 150, 30);
    display.flush ();
  }


  public static void main (String [] args) throws Exception {
    new Image (args).exec ();
  }
}
