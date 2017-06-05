package gnu.x11;


/** Line segment in geometry. */
public class Segment {
  public int x1, y1, x2, y2;


  public Segment (int x1, int y1, int x2, int y2) {
    this.x1 = x1;
    this.y1 = y1;
    this.x2 = x2;
    this.y2 = y2;
  }


  public String toString () {
    return "#Segment [(" + x1 + ", " + y1 + ") ( " + x2 + ", " + y2 + ")";
  }
}
