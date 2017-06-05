package gnu.x11.image;

import gnu.x11.Display;


public class XBM extends Bitmap {
  public XBM (Display display, int width, int height, int [] xbm) {
    super (display, width, height);

    // take <code>int</code> as parameter to avoid casting in java
    for (int i=0; i<data.length; i++)
      data [i] = (byte) xbm [i];
  }
}
