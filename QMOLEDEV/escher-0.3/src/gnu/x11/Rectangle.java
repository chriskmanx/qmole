package gnu.x11;


/** Position and size in geometry. */
public class Rectangle {
  public int x, y, width, height;


  /**
   * Construct from a resource geometry specification string.
   *
   * <ul>supported formats
   * <li>width x height + x + y (e.g. 100x300+50+50 or 100x300-20+20)
   * <li>width x height (e.g. 100x300)
   * <li>+ x + y (+50+50 or -20-20)
   * </ul>
   */
  public Rectangle (String spec) {
    try {
      int i0 = spec.indexOf ('x');
      int i1 = spec.indexOf ('+');
      int i2 = spec.indexOf ('-');
      if (i0 == -1 && i1 == -1 && i2 == -1) throw new RuntimeException ();

      int index;                // beginning of +x+y
      if (i1 == -1) index = i2;
      else if (i2 == -1) index = i1;
      else index = Math.min (i1, i2);


      if (i0 != -1) {        
        int j = index == -1 ? spec.length () : index;

        width = Integer.parseInt (spec.substring (0, i0));
        height = Integer.parseInt (spec.substring (i0+1, j));
      }


      if (index != -1) {
        String offset = spec.substring (index+1, spec.length ());
        int j0 = offset.indexOf ('+');
        int j1 = offset.indexOf ('-');
        int j = Math.max (j0, j1); // beginning of +y

        int s0 = spec.charAt (index) == '+' ? 1 : -1;
        int s1 = offset.charAt (j) == '+' ? 1 : -1;
        
        x = s0 * Integer.parseInt (offset.substring (0, j));
        y = s1 * Integer.parseInt (offset.substring (j+1, offset.length ()));
      }

    } catch (RuntimeException e) {
      throw new RuntimeException ("Invalid geometry specification: "
        + "\nSupported examples:"
        + "\n  100x300+50+50 or 100x300-20+20"
        + "\n  100x300"
        + "\n  +50+50 or -20+20");
    }
  }


  public Rectangle (int x, int y, int width, int height) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }


  public Rectangle within (Rectangle bound) {
    int x0 = Math.max (x, bound.x);
    int y0 = Math.max (y, bound.y);
    int width0 = Math.min (width, bound.width);
    int height0 = Math.min (height, bound.height);
    return new Rectangle (x0, y0, width0, height0);
  }


  public String spec () {
    return width + "x" + height
      + (x >= 0 ? "+" : "") + x
      + (y >= 0 ? "+" : "") + y;    
  }


  public String toString () {
    return "#Rectangle " + spec ();
  }
}
