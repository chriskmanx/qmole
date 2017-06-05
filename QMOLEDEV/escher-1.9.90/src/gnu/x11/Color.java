package gnu.x11;


/** X color. */
public class Color {
  private String name;
  private int pixel;
  private RGB exact, visual;


  public Color (int pixel) {
    this.pixel = pixel;
  }


  public String toString () {
    String name0 = name == null ? "" : name + " ";
    return "#Color " + name0 + "[" + pixel + " " + exact + "]";
  }



  // Set and Gets
  
    public String getName() {

        return name;
    }

    public void setName(String name) {

        this.name = name;
    }

    public int getPixel() {

        return pixel;
    }

    public void setPixel(int pixel) {

        this.pixel = pixel;
    }

    public RGB getExact() {

        return exact;
    }

    public void setExact(RGB exact) {

        this.exact = exact;
    }

    public RGB getVisual() {

        return visual;
    }

    public void setVisual(RGB visual) {

        this.visual = visual;
    }

}
