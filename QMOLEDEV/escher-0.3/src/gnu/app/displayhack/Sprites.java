package gnu.app.displayhack;

import gnu.x11.GC;
import gnu.x11.Pixmap;
import gnu.x11.extension.render.DrawablePicture;
import gnu.x11.extension.render.PictFormat;
import gnu.x11.extension.render.Picture;
import gnu.x11.extension.render.Render;


/**
 * Moving sprites to blend colors.
 *
 * <p>Modified from <code>sprite.c</code> in <a
 * href="http://xfree86.org/~keithp/download/test.tar.bz2">render sample
 * code</a> by Keith Packard.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Sprites.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Sprites.help">
 * help output</a>
 */
public class Sprites extends DisplayHack {
  public static final int COUNT = 5;

  public GC back_buffer_gc, sprite_gc;
  public Picture back_buffer_picture;
  public Pixmap back_buffer;
  public Render render;
  public Sprite [] sprites = new Sprite [COUNT];
  

  class Sprite {
    public float x, y, dx, dy;
    public int color, width, height;
    public Pixmap pixmap;
    public Picture picture;
  
    
    public Sprite (float dx, float dy, int color, int width, int height,
      PictFormat pf) {
      
      x = window.width/2;
      y = window.height/2;
      this.dx = dx;
      this.dy = dy;
      this.color = color;
      this.width = width;
      this.height = height;

      pixmap = new Pixmap (display.default_root, width, height, 
        pf.depth ());
      picture = render.create_picture (pixmap, pf,
        DrawablePicture.Attributes.EMPTY); 

      if (sprite_gc == null)
        sprite_gc = new GC (pixmap);      
    
      sprite_gc.set_foreground (0);
      pixmap.rectangle (sprite_gc, 0, 0, width, height, true);
      sprite_gc.set_foreground (color);
      pixmap.fill_arc (sprite_gc, 0, 0, width, height, 0, 360 * 64);


      for (int i=0; i<8; i++) {
        sprite_gc.set_foreground (divide_color (color, 1<<(7-i)));
        pixmap.fill_arc (sprite_gc, i, i, width-2*i, height-2*i, 0, 360 * 64);
      }
    }


    public void clear () {
      back_buffer.rectangle (back_buffer_gc, (int) x, (int) y, 
        width, height, true);
    }

    
    public void move () {
      // instead of += dx or += dy in C
      x += 10*dx;
      if (x + width > window.width) {
        x = window.width - width;
        dx = -dx;
      } else if (x < 0) {
        x = -x;
        dx = -dx;
      }

      y += 10*dy;
      if (y + height > window.height) {
        y = window.height - height;
        dy = -dy;
      } else if (y < 0) {
        y = -y;
        dy = -dy;
      }

      render.composite (Render.OVER, picture, Picture.NONE,
        back_buffer_picture, 0, 0, 0, 0, (int) x, (int) y, width, height);
    }
  }

  
  public Sprites (String [] args) throws gnu.x11.extension.NotFoundException { 
    super (args, false, false, false, 64, 1000);

    about ("0.1", "moving sprites to blend colors",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    render = new Render (display);   
    PictFormat.Template pf0 = new PictFormat.Template ();
    PictFormat pf1;
 
    int depth = display.default_screen.root_depth ();
    back_buffer_gc = new GC (window);
    back_buffer_gc.set_foreground (display.default_white);
    back_buffer = new Pixmap (window, depth);
    back_buffer.fill_rectangle (back_buffer_gc, 0, 0, back_buffer.width,
                                back_buffer.height);


    // back buffer picture (TODO: find visual)
    pf0.clear ();
    pf0.set_depth (depth);
    pf1 = render.picture_format (pf0, true);

    back_buffer_picture = render.create_picture (back_buffer, pf1,
      DrawablePicture.Attributes.EMPTY);


    // sprite picture format
    pf0.clear ();
    pf0.set_depth (32);
    pf0.set_type (PictFormat.Type.DIRECT);
    pf1 = render.picture_format (pf0, true);
    
    sprites [0] = new Sprite (1.0f, 1.0f, 0x7f7f0000, 150, 150, pf1);
    sprites [1] = new Sprite (-0.66f, 1.0f, 0x40004040, 300, 75, pf1);
    sprites [2] = new Sprite (1.0f, -1.2f, 0xc0c000c0, 75, 300, pf1);
    sprites [3] = new Sprite (1.0f, -1.2f, 0x90909000, 100, 200, pf1);
    sprites [4] = new Sprite (-1.3f, 0.66f, 0x80404040, 200, 100, pf1);
  }


  public static int divide_color (int color, int d) {
    int r = 0;

    for (int i=0; i<32; i+= 8) {
      int c = (color >> i) & 0xff;
      c = c / d;
      r |= c << i;
    }

    return r;
  } 


  public void paint () {
    for (int i=0; i<COUNT; i++) sprites [i].clear ();
    for (int i=0; i<COUNT; i++) sprites [i].move ();
    window.copy_area (back_buffer, back_buffer_gc, 0, 0, back_buffer.width,
                      back_buffer.height, 0, 0);
  }


  public static void main (String [] args) throws Exception { 
    new Sprites (args).exec (); 
  }
}
