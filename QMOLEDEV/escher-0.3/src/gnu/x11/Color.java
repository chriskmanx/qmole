package gnu.x11;


/** X color. */
public class Color {
  public String name;
  public int pixel;
  public RGB exact, visual;


  public Color (int pixel) {
    this.pixel = pixel;
  }


  public String toString () {
    String name0 = name == null ? "" : name + " ";
    return "#Color " + name0 + "[" + pixel + " " + exact + "]";
  }
}
