package gnu.x11;


/** Point in geometry. */
public class Point {
  private int x, y;


  public Point () {}


  public Point (int x, int y) {
    this.x = x;
    this.y = y;
  }


  public String toString () {
    return "#Point +" + x + "+" + y;
  }

  // Set and Gets
  
  public int getX() {
      return x;
  }

  public int getY() {
      return y;
  }
}
