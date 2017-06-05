/*
 * Speedy.java
 *
 * Created on 7. Mai 2007, 10:57
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package gnu.app;

import gnu.x11.Display;
import gnu.x11.GC;
import gnu.x11.Window;
import java.net.SocketException;

/**
 * A simplistic benchmark for raw rendering throughput.
 *
 * @author Roman (roman@kennke.org)
 */
public class Speedy extends gnu.app.Application {

  /**
   * The default number of iterations.
   */
  private static final int DEFAULT_ITERATIONS = 100000;

  /**
   * The application window.
   */
  private Window window;

  /**
   * The connection.
   */
  private Display display;

  /**
   * The number of iterations for each benchmark.
   */
  private int iterations;

  /** Creates a new instance of Speedy */
  public Speedy (String[] args) {
    super (args);

    iterations = option.intt ("iterations",
                              "the number of iterations for each benchmark",
                               DEFAULT_ITERATIONS, 0, Integer.MAX_VALUE);

    about ("0.1", "A simplistic benchmark for raw rendering throughput",
      "Roman Kennke <roman@kennke.org>",
      "http://escher.sourceforge.net/");

    if (help_option) {
      return;
    }

//    try {
//      System.loadLibrary ("localsocket");
//      gnu.java.net.local.LocalSocketAddress addr = new gnu.java.net.local.LocalSocketAddress ("/tmp/.X11-unix/X0");
//      gnu.java.net.local.LocalSocket s = new gnu.java.net.local.LocalSocket (addr);
//      display = new Display (s, "localhost", 0, 0);
//    } catch (SocketException ex) {
//      ex.printStackTrace ();
//    }
    display = new Display ();
    Window.Attributes atts = new Window.Attributes ();
    window = new Window (display.default_root, 0, 0, 600, 400, 0, atts);
    window.map ();

    drawLines (iterations, false);
    drawLines (iterations, true);
    drawRectangles (iterations, false);
    drawRectangles (iterations, true);
    fillRectangles (iterations, false);
    fillRectangles (iterations, true);
  }

  private void drawLines (int iter, boolean record) {
    if (record) {
      System.out.print("draw " + iter + " lines : ");
    }
    long start = System.currentTimeMillis ();
    GC gc = new GC (window);
    for (int i = 0; i < iter; i++) {
      int x = (int) (Math.random () * 640);
      int y = (int) (Math.random () * 400);
      int w = (int) (Math.random () * 640);
      int h = (int) (Math.random () * 400);
      gc.set_foreground ((int) (Math.random () * 0x1000000));
      window.line (gc, x, y, w, h);
    }
    sync ();
    if (record) {
      long end = System.currentTimeMillis ();
      System.out.println ("" + (end - start) + " ms");
    }
  }

  private void drawRectangles (int iter, boolean record) {
    if (record) {
      System.out.print("draw " + iter + " rectangles : ");
    }
    long start = System.currentTimeMillis ();
    GC gc = new GC (window);
    for (int i = 0; i < iter; i++) {
      int x = (int) (Math.random () * 640);
      int y = (int) (Math.random () * 400);
      int w = (int) (Math.random () * (640 - x));
      int h = (int) (Math.random () * (400 - y));
      gc.set_foreground ((int) (Math.random () * 0x1000000));
      window.rectangle (gc, x, y, w, h, false);
    }
    sync ();
    if (record) {
      long end = System.currentTimeMillis ();
      System.out.println ("" + (end - start) + " ms");
    }
  }

  private void fillRectangles (int iter, boolean record) {
    if (record) {
      System.out.print("fill " + iter + " rectangles : ");
    }
    long start = System.currentTimeMillis ();
    GC gc = new GC (window);
    for (int i = 0; i < iter; i++) {
      int x = (int) (Math.random () * 640);
      int y = (int) (Math.random () * 400);
      int w = (int) (Math.random () * (640 - x));
      int h = (int) (Math.random () * (400 - x));
      gc.set_foreground ((int) (Math.random () * 0x1000000));
      window.rectangle (gc, x, y, w, h, true);
    }
    sync ();
    if (record) {
      long end = System.currentTimeMillis ();
      System.out.println ("" + (end - start) + " ms");
    }
  }

  /**
   * Synces with the X server by forcing a reply. This way we make sure
   * that everything is completely drawn before measuring the time.
   */
  private void sync () {
    display.check_error ();
  }

  /**
   * Starts the program.
   */
  public static void main (String[] args) {
    new Speedy(args);
  }
}
