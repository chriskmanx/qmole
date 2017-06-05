package gnu.x11;


/** Arc in geometry. */
public class Arc {
  public int x, y, width, height, angle1, angle2;


  public Arc (int x, int y, int width, int height, 
    int angle1, int angle2) {

    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.angle1 = angle1;
    this.angle2 = angle2;
  }


  public String toString () {
    return "#Arc " + (new Rectangle (x, y, width, height)).spec ()
      + "@" + angle1 + "@" + angle2;
  }
}
