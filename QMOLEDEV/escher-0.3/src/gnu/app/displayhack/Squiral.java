package gnu.app.displayhack;


/** 
 * Square spirals. It displays interacting, spiral-producing automata.
 *
 * <p>Modified from <code>squiral.c</code> in <a href=
 * "http://www.jwz.org/xscreensaver/">xscreensaver</a> by Jeff Epler
 * <jepler@inetnebr.com>. Object-oriented-ized. Cyclic color not yet
 * implemented.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Squiral.gif">
 * screenshot 8</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/displayhack/Squiral.help">
 * help output</a>
 */
public class Squiral extends DisplayHack {
  public static final int NORTH = 0;
  public static final int EAST = 1;
  public static final int SOUTH = 2;
  public static final int WEST = 3;


  /** x increment values for different directions. */
  public static final int [] DX = new int [4];
  static {
    DX [NORTH] = 0;
    DX [EAST] = 1;
    DX [SOUTH] = 0;
    DX [WEST] = -1;
  }


  /** y increment values for different directions. */
  public static final int [] DY = new int [4];
  static {
    DY [NORTH] = 1;
    DY [EAST] = 0;
    DY [SOUTH] = -1;
    DY [WEST] = 0;
  }


  public class Worm {
    public gnu.x11.Color color;
    public boolean left_handed;
    public int x, y;

    /**
     * Direction of its head. Valid:
     * 0 (north),
     * 1 (east),
     * 2 (south),
     * 3 (west).
     */
    public int direction;
    
    
    public Worm () {
      direction = random.nextInt (4);
      left_handed = chance (Squiral.this.left_handed);
      init ();
    }


    public int clockwise () {
      // +1 = next direction
      return (direction + 1) % 4;
    }


    public int counter_clockwise () {
      // -1 = previous direction
      // +4 = no negative value
      return (direction - 1 + 4) % 4;
    }

    
    public void go () {
      if (chance (disorder)) 
        left_handed = chance (Squiral.this.left_handed);

      if ((left_handed          // left-handed
        && !try_go (counter_clockwise ())
        && !try_go (direction)  // straight
        && !try_go (clockwise ()))
        
        || (!left_handed        // right-handed
          && !try_go (clockwise ())
          && !try_go (direction) // straight
          && !try_go (counter_clockwise ())))

        // stuck, raise a new worm
        init ();
    }


    public void go (int x, int y) {
      // within window bound
      int in_x = (x + window.width) % window.width;
      int in_y = (y + window.height) % window.height;

      filled [in_y * window.width + in_x] = true;
      window.point (gc, in_x, in_y);
      filled_count++;
    }


    public void init () {
      x = random.nextInt (window.width);
      y = random.nextInt (window.height);
      color = random_color ();
    }


    /**
     * @return true if successful
     */
    public boolean try_go (int direction) {
      int dx = DX [direction];
      int dy = DY [direction];

      // if road ahead is clear
      if (!clear (x+dx, y+dy) || !clear (x+dx+dx, y+dy+dy)) return false;
      
      gc.set_foreground (color);
      go (x+dx, y+dy);
      go (x+dx+dx, y+dy+dy);

      this.direction = direction;
      x = (x + dx + dx + window.width) % window.width;
      y = (y + dy + dy + window.height) % window.height;
      return true;
    }
  }


  public float disorder, fill, left_handed;
  public long draw_delay;
  public int worm_count, filled_count;
  public Worm [] worms;


  /** If a pixel in window is filled. */
  public boolean [] filled, EMPTY_FILLED;


  public Squiral (String [] args) { 
    super (args, true, true, false, 100, 2000);

    disorder = option.scale ("disorder",
      "chance a worm will choose a new direction", 0.005f);
    draw_delay = option.longg ("draw-delay", 
      "delay between iterations in millis", 10);
    fill = option.floatt ("fill",
      "how much screen space should be filled",
      0.75f, 0.01f, 0.99f);
    left_handed = option.scale ("left-handed",
      "chance a worm goes left hand", 0.5f);
    worm_count = option.intt ("worm-count",
      "total number of worms: 0 = window.width/32", 0);

    about ("0.1", "square spirals",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    if (worm_count == 0) worm_count = window.width/32;
    EMPTY_FILLED = new boolean [window.width * window.height];
    filled = new boolean [window.width * window.height];
    worms = new Worm [worm_count];    
    for (int i=0; i<worm_count; i++) worms [i] = new Worm ();    
  }


  /** If a pixel is not filled. */
  public boolean clear (int x, int y) {
    // within window bound
    int in_x = (x + window.width) % window.width;
    int in_y = (y + window.height) % window.height;
    return !filled [in_y * window.width + in_x];
  }


  public void init () {
    System.arraycopy (EMPTY_FILLED, 0, filled, 0, filled.length);
    filled_count = 0;
  }


  public void paint () {
    init ();
    while (true) {        
      for (int i=0; i<worm_count; i++) worms [i].go ();
      if (filled_count > fill * filled.length) return;
      if (sleep (draw_delay)) return;
    }
  }


  public static void main (String [] args) {
    new Squiral (args).exec ();
  }
}
