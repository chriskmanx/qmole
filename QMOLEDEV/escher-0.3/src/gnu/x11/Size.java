package gnu.x11;


/** Width and height in geometry. */
public class Size {
  public int width, height;


  public Size (int width, int height) {
    this.width = width;
    this.height = height;
  }


  public String toString () {
    return "#Size " +  width + "x" + height;
  }
}
