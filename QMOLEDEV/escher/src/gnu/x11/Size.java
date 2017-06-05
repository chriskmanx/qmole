package gnu.x11;


/** Width and height in geometry. */
public class Size {
  private int width, height;


  public Size (int width, int height) {
    this.width = width;
    this.height = height;
  }


  public String toString () {
    return "#Size " +  width + "x" + height;
  }
  
  
  public int getHeight() {

    return height;
  }

  public int getWidth() {

    return width;
  }
}
