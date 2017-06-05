package gnu.x11;


/** Text item for drawing text to X server. */
public class Text {
  public String s;
  public int delta;
  public Font font;
  

  public Text (String s) { this.s = s; }

  public Text (String s, int delta) {
    this.s = s;
    this.delta = delta;
  }


  public Text (String s, int delta, Font font) {
    this.s = s;
    this.delta = delta;
    this.font = font;
  }


  public int length (int bit) {

    // 2 = length of string field and delta field
    // 5 = font field
    int n = font == null ? 2 : 5;

    if (bit == 8 || s.charAt (0) > 128) // non-ascii	  
      n = n + s.length ();
    else
      n = n + 2 * s.length ();

    return n;
  }


  public String toString () {
    return "#Text [" + s + " " + delta + " " + font + "]";
  }
}
