package gnu.x11;

import gnu.x11.image.Image;
import gnu.x11.image.ZPixmap;


/** X drawable. */
public abstract class Drawable extends Resource {

  public int width, height;


  /** Predefined. */
  public Drawable (int id) {
    super (id);
  }


  /** Create. */
  public Drawable (Display display) {
    super (display);
  }


  /** Intern. */
  public Drawable (Display display, int id) {
    super (display, id);
  }

  public static class GeometryInfo {

    public int depth;
    public int root_window_id;
    public int x;
    public int y;
    public int width;
    public int height;
    public int border_width;

    GeometryInfo (ResponseInputStream i) {
      depth = i.read_int8 ();
      i.skip (6);
      root_window_id = i.read_int32 ();
      x = i.read_int16 ();
      y = i.read_int16 ();
      width = i.read_int16 ();
      height = i.read_int16 ();
      border_width = i.read_int16 ();
      i.skip (10); // Unused.
    }
  }

  // opcode 14 - get geometry
  /**
   * @see <a href="XGetGeometry.html">XGetGeometry</a>
   */
  public GeometryInfo get_geometry () {

    GeometryInfo info;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (14, 0, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new GeometryInfo (i);
      }
    }
    // FIXME: Is this good? Not sure.
    width = info.width;
    height = info.height;
    return info;
  }

  // opcode 62 - copy area
  /**
   * Copies a specified rectangular area to another location.
   *
   * @param src the source drawable
   * @param gc the GC for the operation
   * @param src_x the source rectangle, x coordinate
   * @param src_y the source rectangle, y coordinate
   * @param width the width of the area to copy
   * @param height the height of the area to copy
   * @param dst_x the destination rectangle, x coordinate
   * @param dst_y the destination rectangle, y coordinate
   *
   * @see <a href="XCopyArea.html">XCopyArea</a>
   */
  public void copy_area (Drawable src, GC gc, int src_x, int src_y, 
                         int width, int height, int dst_x, int dst_y) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (62, 0, 7);
      o.write_int32 (src.id); // Src-drawable.
      o.write_int32 (id);     // Dst-drawable.
      o.write_int32 (gc.id);  // GC.
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int16 (dst_x);
      o.write_int16 (dst_y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }


  // opcode 63 - copy plane
  /**
   * @see <a href="XCopyPlane.html">XCopyPlane</a>
   */
  public void copy_plane (Drawable src, GC gc, int src_x, int src_y, 
    int dst_x, int dst_y, int width, int height, int bit_plane) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (63, 0, 8);
      o.write_int32 (src.id);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int16 (dst_x);
      o.write_int16 (dst_y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int16 (bit_plane);
      o.send ();
    }
  }
    

  /**
   * Coordinate mode ORIGIN, specifies that points are always considered
   * relative to the origin.
   */
  public static final int ORIGIN = 0;

  /**
   * Coordinate mode PREVIOUS, specifies that points are considered relative
   * to the previous point (where the first point is usually considered
   * relative to the origin).
   */
  public static final int PREVIOUS = 1;

  // opcode 64 - poly point
  /**
   * Draws multiple points.
   *
   * @param gc the GC to use
   * @param xpoints the points' x coordinates
   * @param ypoints the points' y coodinates
   * @param npoints the number of points
   * @param coordinate_mode valid: {@link #ORIGIN}, {@link #PREVIOUS}
   * 
   * @see <a href="XDrawPoints.html">XDrawPoints</a>
   */
  public void poly_point (GC gc, int[] xpoints, int[] ypoints, int npoints,
                          int coordinate_mode) {

    // FIXME: Handle aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (64, coordinate_mode, 3 + npoints);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (xpoints[i]);
        o.write_int16 (ypoints[i]);
      }
      o.send ();
    }
  }

  /**
   * Draws multiple points.
   *
   * @param gc the GC to use
   * @param xpoints the points' x coordinates
   * @param ypoints the points' y coodinates
   * @param npoints the number of points
   * @param coordinate_mode valid: {@link #ORIGIN}, {@link #PREVIOUS}
   * 
   * @see <a href="XDrawPoints.html">XDrawPoints</a>
   */
  public void poly_point (GC gc, Point[] points, int coordinate_mode) {

    // FIXME: Handle aggregation.
    int npoints = points.length;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (64, coordinate_mode, 3 + points.length);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (points[i].x);
        o.write_int16 (points[i].y);
      }
      o.send ();
    }
  }

  // opcode 65 - poly line
  /**
   * Draws multiple lines that connect the specified points.
   *
   * @param gc the GC to use
   * @param xpoints the points' x coordinates
   * @param ypoints the points' y coodinates
   * @param npoints the number of points
   * @param coordinate_mode valid: {@link #ORIGIN}, {@link #PREVIOUS}
   * 
   * @see <a href="XDrawLines.html">XDrawLines</a>
   */
  public void poly_line (GC gc, int[] xpoints, int[] ypoints, int npoints,
                         int coordinate_mode) {

    // FIXME: Handle aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (65, coordinate_mode, 3 + npoints);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (xpoints[i]);
        o.write_int16 (ypoints[i]);
      }
      o.send ();
    }
  }

  public void poly_line (GC gc, int[] xpoints, int[] ypoints, int npoints,
                         int coordinate_mode, boolean close) {

    // FIXME: Handle aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (65, coordinate_mode, 3 + npoints + (close ? 1 : 0));
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (xpoints[i]);
        o.write_int16 (ypoints[i]);
      }
      if (close) {
        o.write_int16 (xpoints [0]);
        o.write_int16 (ypoints [0]);
      }
      o.send ();
    }
  }

  /**
   * Draws multiple lines which connect the specified points.
   *
   * @param gc the GC to use
   * @param points the points that make up the lines
   * @param coordinate_mode valid: {@link #ORIGIN}, {@link #PREVIOUS}
   * 
   * @see <a href="XDrawLines.html">XDrawLines</a>
   */
  public void poly_line (GC gc, Point[] points, int coordinate_mode) {

    // FIXME: Handle aggregation.
    int npoints = points.length;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (65, coordinate_mode, 3 + points.length);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (points[i].x);
        o.write_int16 (points[i].y);
      }
      o.send ();
    }
  }

  // opcode 66 - poly segment
  /**
   * Draws multiple line segments
   *
   * @param gc the GC to use
   * @param segments the line segments to draw
   *
   * @see <a href="XDrawSegments.html">XDrawSegments</a>
   */
  public void poly_segment (GC gc, Segment [] segments) {

    // FIXME: Handle aggregation.

    int nsegs = segments.length;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (66, 0, 3 + 2 * nsegs);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < nsegs; i++) {
        Segment seg = segments[i];
        o.write_int16 (seg.x1);
        o.write_int16 (seg.y1);
        o.write_int16 (seg.x2);
        o.write_int16 (seg.y2);
      }
      o.send ();
    }
  }

  // opcode 67 - poly rectangle
  /**
   * Draws the outline of multiple rectangles.
   *
   * @param gc the GC to use
   * @param rectangles the rectangles to draw
   *
   * @see <a href="XDrawRectangles.html">XDrawRectangles</a>
   * @see <a href="XFillRectangles.html">XFillRectangles</a>
   * @see Request.Aggregate aggregation
   */
  public void poly_rectangle (GC gc, Rectangle [] rectangles) {

    // FIXME: Handle aggregation.

    int nrects = rectangles.length;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (67, 0, 3 + 2 * nrects);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < nrects; i++) {
        Rectangle rect = rectangles[i];
        o.write_int16 (rect.x);
        o.write_int16 (rect.y);
        o.write_int16 (rect.width);
        o.write_int16 (rect.height);
      }
      o.send ();
    }
  }

  // opcode 68 - poly arc
  public void poly_arc (GC gc, Arc [] arcs) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (68, 0, 3 + 3 * arcs.length);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < arcs.length; i++) {
        Arc arc = arcs [i];
        o.write_int16 (arc.x);
        o.write_int16 (arc.y);
        o.write_int16 (arc.width);
        o.write_int16 (arc.height);
        o.write_int16 (arc.angle1);
        o.write_int16 (arc.angle2);
      }
    }
  }

  // opcode 71 - poly fill arc
  public void poly_fill_arc (GC gc, Arc [] arcs) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (71, 0, 3 + 3 * arcs.length);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      for (int i = 0; i < arcs.length; i++) {
        Arc arc = arcs [i];
        o.write_int16 (arc.x);
        o.write_int16 (arc.y);
        o.write_int16 (arc.width);
        o.write_int16 (arc.height);
        o.write_int16 (arc.angle1);
        o.write_int16 (arc.angle2);
      }
    }
  }

  public static final int COMPLEX = 0;
  public static final int NONCONVEX = 1;
  public static final int CONVEX = 2;


  // opcode 69 - fill poly
  /**
   * This request will be aggregated.
   * 
   * @param shape valid:
   * {@link #COMPLEX},
   * {@link #NONCONVEX},
   * {@link #CONVEX}
   * 
   * @param coordinate_mode valid:
   * {@link #ORIGIN},
   * {@link #PREVIOUS}
   * 
   * @see <a href="XFillPolygon.html">XFillPolygon</a>
   * @see Request.Aggregate aggregation
   */
  public void fill_poly (GC gc, Point [] points, int shape,
                         int coordinate_mode) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (69, 0, 4 + points.length);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int8 (shape);
      o.write_int8 (coordinate_mode);
      o.skip (2);
      for (int i = 0; i < points.length; i++) {
        Point p = points [i];
        o.write_int16 (p.x);
        o.write_int16 (p.y);
      }
      o.send ();
    }
  }

  public void fill_poly (GC gc, int [] xpoints, int [] ypoints, int npoints,
                         int shape, int coordinate_mode) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (69, 0, 4 + npoints);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int8 (shape);
      o.write_int8 (coordinate_mode);
      o.skip (2);
      for (int i = 0; i < npoints; i++) {
        o.write_int16 (xpoints [i]);
        o.write_int16 (ypoints [i]);
      }
    }
  }

  // opcode 72 - put image
  public void put_small_image (GC gc, Image image, int y1, int y2, 
                               int x, int y) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      int offset = image.get_line_byte_count () * y1;
      int length = image.get_line_byte_count () * (y2 - y1);
      int p = RequestOutputStream.pad (length);

      Image.Format format = image.get_format ();
      o.begin_request (72, format.id (), 6 + (length + p) / 4);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (image.get_width ());
      o.write_int16 (y2 - y1);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int8 (image.get_left_pad ());
      o.write_int8 (image.get_pixmap_format ().depth);
      o.skip (2);
      o.write (image.get_data (), offset, length);
      o.send ();
    }
  }


  // opcode 73 - get image TODO
  /**
   * @see <a href="XGetImage.html">XGetImage</a>
   */
  public Image image (int x, int y, int width, int height, int plane_mask,
                      Image.Format format) {
//    System.err.println("params x: " + x + ", y: " + y + ", w: " + width + ", h: " + height + ", plane_mask: " + plane_mask + ", format: " + format);
//    System.err.println("drawable " + this + ",  w: " + this.width + ", h: " + this.height);
    RequestOutputStream o = display.out;
    Image image;
    synchronized (o) {
      o.begin_request (73, format.id (), 5);
      o.write_int32 (id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int32 (plane_mask);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        int depth = i.read_int8 ();
        i.skip (2);
        int len = i.read_int32 () * 4;
        int visual_id = i.read_int32 ();
        i.skip (20);
        byte[] data = new byte [len];
        i.read_data (data);
        // TODO Handle XYPixmap.
        if (format == Image.Format.ZPIXMAP) {
          image = new ZPixmap (display, width, height,
                               display.default_pixmap_format, data);
        } else {
          throw new UnsupportedOperationException("Support for XYPixmap not yet implemented");
        }
      }
    }
    return image;
  }


  // opcode 74 - poly text8
  /**
   * @see <a href="XDrawText.html">XDrawText</a>
   */
  public void poly_text (GC gc, int x, int y, Text [] texts) {

    int n = length (texts, 8);
    int p = RequestOutputStream.pad (n);
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (74, 0, 4 + (n + p) / 4);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);

      for (int i = 0; i < texts.length; i++) {
        if (texts [i].font != null) {
          o.write_int8 (255);// font-shift indicator
          o.write_int32 (texts [i].font.id); // java = MSB
        }
        o.write_int8 (texts [i].s.length ());
        o.write_int8 (texts [i].delta);
        o.write_string8 (texts [i].s);
      }
      // Can't simply skip the padding bytes, otherwise the X server
      // would think that there are more items if the next byte in the
      // buffer is != 0, this would produce random errors.
      for (int i = 0; i < p; i++) {
        o.write_int8 (0);
      }
      o.send ();
    }
  }


  // opcode 75 - poly text16
  /**
   * @see <a href="XDrawText16.html">XDrawText16</a>
   */
  public void poly_text16 (GC gc, int x, int y, Text [] texts) {

    int n = length (texts, 16);
    int p = RequestOutputStream.pad (n);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (75, 0, 4 + (n + p) / 4);

      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);

      for (int i = 0; i < texts.length; i++) {
        if (texts [i].font != null) {
          o.write_int8 (255);// font-shift indicator
          o.write_int32 (texts [i].font.id); // java = MSB
        }

        String s = texts [i].s;

        if (s.charAt (0) > 128) { // non-ascii
          o.write_int8 (s.length ()/2);
          o.write_int8 (texts [i].delta);
          o.write_string8 (s);
        } else {// ascii
          o.write_int8 (s.length ());
          o.write_int8 (texts [i].delta);
          o.write_string16 (s);
        }
      }

      o.send ();
      
    }
  }



  // opcode 76 - image text8
  /**
   * @see <a href="XDrawImageString.html">XDrawImageString</a>
   */
  public void image_text (GC gc, int x, int y, String s) {

    int n = s.length ();
    int p = RequestOutputStream.pad (n);
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (76, n, 4 + (n + p) / 4);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_string8 (s);
      o.send ();
    }
  }


  // opcode 77 - image text16
  /**
   * @see <a href="XDrawImageString16.html">XDrawImageString16</a>
   */
  public void image_text16 (GC gc, int x, int y, String s) {

    int n = s.length ();
    int p = RequestOutputStream.pad (2 * n);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (77, n, 4 + (2 * n + p) / 4);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_string16 (s);
      o.send ();
    }
  }


  public static final int CURSOR = 0;
  public static final int TILE = 1;
  public static final int STIPPLE = 2;

  // opcode 97 - query best size
  /**
   * @param klass valid:
   * {@link #CURSOR},
   * {@link #TILE},
   * {@link #STIPPLE}
   * 
   * @see <a href="XQueryBestSize.html">XQueryBestSize</a>
   */
  public Size best_size (int klass, int width, int height) {
    RequestOutputStream o = display.out;
    int w, h;
    synchronized (o) {
      o.begin_request (97, klass, 3);
      o.write_int32 (id);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();

      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply(o);
        i.skip (8);
        w = i.read_int16 ();
        h = i.read_int16 ();
        i.skip (20);
      }
    }
    return new Size (w, h);
  }

  /**
   * Draws the outline of a single arc.
   *
   * @param gc the GC to use
   * @param x the bounding rectangle, x coordinate
   * @param y the bounding rectangle, y coordinate
   * @param w the bounding rectangle, width
   * @param h the bounding rectangle, height
   * @param angle1 the start angle, from 3 o'clock ccw, in degrees
   * @param angle2 the span angle, from angle1 ccw, in degrees
   *
   * @see #poly_arc(GC, Arc[])
   */
  public void arc (GC gc, int x, int y, int width, int height, 
                   int angle1, int angle2) {

    // FIXME: Handle aggregation.

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (68, 0, 6);
      o.write_int32 (id);
      o.write_int32 (gc.id);

      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int16 (angle1);
      o.write_int16 (angle2);
      o.send ();
    }
  }

  /**
   * Fills a single arc.
   *
   * @param gc the GC to use
   * @param x the bounding rectangle, x coordinate
   * @param y the bounding rectangle, y coordinate
   * @param w the bounding rectangle, width
   * @param h the bounding rectangle, height
   * @param angle1 the start angle, from 3 o'clock ccw, in degrees
   * @param angle2 the span angle, from angle1 ccw, in degrees
   *
   * @see #poly_arc(GC, Arc[])
   */
  public void fill_arc (GC gc, int x, int y, int width, int height, 
                        int angle1, int angle2) {

    // FIXME: Handle aggregation.

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (71, 0, 6);
      o.write_int32 (id);
      o.write_int32 (gc.id);

      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int16 (angle1);
      o.write_int16 (angle2);
      o.send ();
    }
  }

  /** 
   * Draws a single line.
   *
   * @param gc the GC to use
   * @param x1 the start point, x coordinate
   * @param y1 the start point, y coordinate
   * @param x2 the end point, x coordinate
   * @param y2 the end point, y coordinate
   */
  public void line (GC gc, int x1, int y1, int x2, int y2) {

    // FIXME: Handle aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (65, ORIGIN, 5);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x1);
      o.write_int16 (y1);
      o.write_int16 (x2);
      o.write_int16 (y2);
    }
  }

  public void segment (GC gc, int x1, int y1, int x2, int y2) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      if (o.current_opcode () == 66 && o.fits (8)) {
        o.increase_length (2);
        o.write_int16 (x1);
        o.write_int16 (y1);
        o.write_int16 (x2);
        o.write_int16 (y2);
      } else {
        o.begin_request (66, ORIGIN, 5);
        o.write_int32 (id);
        o.write_int32 (gc.id);
        o.write_int16 (x1);
        o.write_int16 (y1);
        o.write_int16 (x2);
        o.write_int16 (y2);
      }
    }
  }

  /**
   * Draws a single point.
   *
   * @param x the x coordinate
   * @param y the y coordinate
   */
  public void point (GC gc, int x, int y) {
    // FIXME: Handle aggregation.
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (64, ORIGIN, 4);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.send ();
    }
  }

  public void put_image (GC gc, Image image, int x, int y) {
    
    // TODO: Make use of big requests when possible.
    int max_data_byte = display.maximum_request_length - 24;
    int lbc = image.get_line_byte_count ();
    int request_height = lbc > 0 ? max_data_byte / image.get_line_byte_count ()
                                 : 1;
    int rem = image.get_height () % request_height;
    int request_count = image.get_height ()/ request_height + (rem == 0 ? 0 : 1);

    for (int i = 0; i < request_count; i++) {
      put_small_image (gc, image, i * request_height,
                       Math.min (image.get_height (), (i + 1) * request_height), x,
                       y + i * request_height);
    }
  }

  /**
   * Draws a single rectangle.
   *
   * @param gc the graphic context
   * @param x the upper left corner, x coordinate
   * @param y the upper left corner, y coordinate
   * @param width the width
   * @param height the height
   *
   * @see #poly_rectangle(GC, Rectangle[])
   */      
  public void rectangle (GC gc, int x, int y, int width, int height) {

    // FIXME: Handle aggregation.

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (67, 0, 5);
      o.write_int32 (id);
      o.write_int32 (gc.id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }

  /**
   * Fills a single rectangle.
   *
   * @param gc the graphic context
   * @param x the upper left corner, x coordinate
   * @param y the upper left corner, y coordinate
   * @param width the width
   * @param height the height
   *
   * @see #poly_fill_rectangle(GC, Rectangle[])
   */      
  public void fill_rectangle (GC gc, int x, int y, int width, int height) {


    RequestOutputStream o = display.out;
    synchronized (o) {
    if (o.current_opcode() == 70 && o.get_int32(4) == id
        && o.get_int32(8) == gc.id)
      {
        o.increase_length(2);
        o.write_int16 (x);
        o.write_int16 (y);
        o.write_int16 (width);
        o.write_int16 (height);
      }
    else
      {
        o.begin_request (70, 0, 5);
        o.write_int32 (id);
        o.write_int32 (gc.id);
        o.write_int16 (x);
        o.write_int16 (y);
        o.write_int16 (width);
        o.write_int16 (height);
        o.send ();
      }
    }
  }

  /**
   * @see #poly_text(GC, int, int, Text[])
   */
  public void text8 (GC gc, int x, int y, String s, int delta, Font font) {
    poly_text (gc, x, y, new Text [] {new Text (s, delta, font)});
  }


  /**
   * @see #text(GC, int, int, String, int, Font)
   */
  public void text (GC gc, int x, int y, String s) {
    poly_text (gc, x, y, new Text [] {new Text (s, 0, null)});
  }

  public void text16 (GC gc, int x, int y, String s) {
    poly_text16 (gc, x, y, new Text [] {new Text (s, 0, null)});
  }

  private int length (Text [] texts, int bit) {
    int n = 0;
    for (int i = 0; i < texts.length; i++)
      n += texts [i].length (bit);
    return n;
  }

  /**
   * @deprecated
   */
  public void rectangle (GC xgc, int x, int y, int w, int h, boolean fill) {
    if (fill)
      fill_rectangle (xgc, x, y, w, h);
    else
      rectangle (xgc, x, y, w, h);
  }
}

