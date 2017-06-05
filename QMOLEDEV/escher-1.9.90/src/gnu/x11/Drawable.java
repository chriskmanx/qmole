
package gnu.x11;

import gnu.x11.Host.ChangeOperation;
import gnu.x11.Host.Shape;
import gnu.x11.image.Image;
import gnu.x11.image.ZPixmap;

/** X drawable. */
public abstract class Drawable extends Resource {

    public int width, height;

    public enum CoordinateMode {
        ORIGIN(0),
        PREVIOUS(1);
        
        private int code;
        
        CoordinateMode(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static CoordinateMode getOperation(int code) {
            return code == 0 ? CoordinateMode.ORIGIN : CoordinateMode.PREVIOUS;
        }
    }
    
    public enum Fill {
        COMPLEX(0),
        NONCONVEX(1),
        CONVEX(2);
        
        private int code;
        
        Fill(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static Fill getFamily(int code) {
            switch (code)
            {
                case 0: return Fill.COMPLEX;
                case 1: return Fill.NONCONVEX;
                case 2: return Fill.CONVEX;
                
                default: return Fill.COMPLEX;
            }
        }
    }
    
    public enum Shape {
        CURSOR(0),
        TILE(1),
        STIPPLE(2);
        
        private int code;
        
        Shape(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static Shape getFamily(int code) {
            switch (code)
            {
                case 0: return Shape.CURSOR;
                case 1: return Shape.TILE;
                case 2: return Shape.STIPPLE;
                
                default: return Shape.CURSOR;
            }
        }
    }
    
    /** Predefined. */
    public Drawable(int id) {

        super(id);
    }

    /** Create. */
    public Drawable(Display display) {

        super(display);
    }

    /** Intern. */
    public Drawable(Display display, int id) {

        super(display, id);
    }

    public static class GeometryInfo {
        // TODO: To private??
        public int depth;

        public int rootWindowID;

        public int x;

        public int y;

        public int width;

        public int height;

        public int borderWidth;

        GeometryInfo(ResponseInputStream i) {

            depth = i.readInt8();
            i.skip(6);
            rootWindowID = i.readInt32();
            x = i.readInt16();
            y = i.readInt16();
            width = i.readInt16();
            height = i.readInt16();
            borderWidth = i.readInt16();
            i.skip(10); // Unused.
        }
    }

    // opcode 14 - get geometry
    /**
     * @see <a href="XGetGeometry.html">XGetGeometry</a>
     */
    public GeometryInfo getGeometry() {

        GeometryInfo info;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                info = new GeometryInfo(i);
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
     * @param src
     *                the source drawable
     * @param gc
     *                the GC for the operation
     * @param srcX
     *                the source rectangle, x coordinate
     * @param srcY
     *                the source rectangle, y coordinate
     * @param width
     *                the width of the area to copy
     * @param height
     *                the height of the area to copy
     * @param dstX
     *                the destination rectangle, x coordinate
     * @param dstY
     *                the destination rectangle, y coordinate
     * 
     * @see <a href="XCopyArea.html">XCopyArea</a>
     */
    public void copyArea(Drawable src, GC gc, int srcX, int srcY, int width,
                          int height, int dstX, int dstY) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(62, 0, 7);
            o.writeInt32(src.id); // Src-drawable.
            o.writeInt32(id); // Dst-drawable.
            o.writeInt32(gc.id); // GC.
            o.writeInt16(srcX);
            o.writeInt16(srcY);
            o.writeInt16(dstX);
            o.writeInt16(dstY);
            o.writeInt16(width);
            o.writeInt16(height);
            o.send();
        }
    }

    // opcode 63 - copy plane
    /**
     * @see <a href="XCopyPlane.html">XCopyPlane</a>
     */
    public void copyPlane(Drawable src, GC gc, int srcX, int srcY,
                           int dstX, int dstY, int width, int height,
                           int bitPlane) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(63, 0, 8);
            o.writeInt32(src.id);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(srcX);
            o.writeInt16(srcY);
            o.writeInt16(dstX);
            o.writeInt16(dstY);
            o.writeInt16(width);
            o.writeInt16(height);
            o.writeInt16(bitPlane);
            o.send();
        }
    }

    // opcode 64 - poly point
    /**
     * Draws multiple points.
     * 
     * @param gc
     *                the GC to use
     * @param xpoints
     *                the points' x coordinates
     * @param ypoints
     *                the points' y coodinates
     * @param npoints
     *                the number of points
     * @param coordinateMode
     *                valid: {@link #ORIGIN}, {@link #PREVIOUS}
     * 
     * @see <a href="XDrawPoints.html">XDrawPoints</a>
     */
    public void polyPoint(GC gc, int[] xpoints, int[] ypoints, int npoints,
                           CoordinateMode coordinateMode) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(64, coordinateMode.getCode(), 3 + npoints);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (int i = 0; i < npoints; i++) {
                o.writeInt16(xpoints[i]);
                o.writeInt16(ypoints[i]);
            }
            o.send();
        }
    }

    /**
     * Draws multiple points.
     * 
     * @param gc
     *                the GC to use
     * @param xpoints
     *                the points' x coordinates
     * @param ypoints
     *                the points' y coodinates
     * @param npoints
     *                the number of points
     * @param coordinate_mode
     *                valid: {@link #ORIGIN}, {@link #PREVIOUS}
     * 
     * @see <a href="XDrawPoints.html">XDrawPoints</a>
     */
    public void polyPoint(GC gc, Point[] points, CoordinateMode coordinateMode) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(64, coordinateMode.getCode(), 3 + points.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (Point p : points) {
                o.writeInt16(p.getX());
                o.writeInt16(p.getY());
            }
            o.send();
        }
    }

    // opcode 65 - poly line
    /**
     * Draws multiple lines that connect the specified points.
     * 
     * @param gc
     *                the GC to use
     * @param xpoints
     *                the points' x coordinates
     * @param ypoints
     *                the points' y coodinates
     * @param npoints
     *                the number of points
     * @param coordinateMode
     *                valid: {@link #ORIGIN}, {@link #PREVIOUS}
     * 
     * @see <a href="XDrawLines.html">XDrawLines</a>
     */
    public void polyLine(GC gc, int[] xpoints, int[] ypoints, int npoints,
                          CoordinateMode coordinateMode) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(65, coordinateMode.getCode(), 3 + npoints);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (int i = 0; i < npoints; i++) {
                o.writeInt16(xpoints[i]);
                o.writeInt16(ypoints[i]);
            }
            o.send();
        }
    }

    public void polyLine(GC gc, int[] xpoints, int[] ypoints, int npoints,
                          CoordinateMode coordinateMode, boolean close) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(65, coordinateMode.getCode(), 3 + npoints + (close ? 1 : 0));
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (int i = 0; i < npoints; i++) {
                o.writeInt16(xpoints[i]);
                o.writeInt16(ypoints[i]);
            }
            if (close) {
                o.writeInt16(xpoints[0]);
                o.writeInt16(ypoints[0]);
            }
            o.send();
        }
    }

    /**
     * Draws multiple lines which connect the specified points.
     * 
     * @param gc
     *                the GC to use
     * @param points
     *                the points that make up the lines
     * @param coordinate_mode
     *                valid: {@link #ORIGIN}, {@link #PREVIOUS}
     * 
     * @see <a href="XDrawLines.html">XDrawLines</a>
     */
    public void polyLine(GC gc, Point[] points, CoordinateMode coordinateMode) {

        // FIXME: Handle aggregation.
        int npoints = points.length;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(65, coordinateMode.getCode(), 3 + points.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (int i = 0; i < npoints; i++) {
                o.writeInt16(points[i].getX());
                o.writeInt16(points[i].getY());
            }
            o.send();
        }
    }

    // opcode 66 - poly segment
    /**
     * Draws multiple line segments
     * 
     * @param gc
     *                the GC to use
     * @param segments
     *                the line segments to draw
     * 
     * @see <a href="XDrawSegments.html">XDrawSegments</a>
     */
    public void polySegment(GC gc, Segment[] segments) {

        // FIXME: Handle aggregation.

        int nsegs = segments.length;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(66, 0, 3 + 2 * nsegs);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (int i = 0; i < nsegs; i++) {
                Segment seg = segments[i];
                o.writeInt16(seg.getX1());
                o.writeInt16(seg.getY1());
                o.writeInt16(seg.getX2());
                o.writeInt16(seg.getY2());
            }
            o.send();
        }
    }

    // opcode 67 - poly rectangle
    /**
     * Draws the outline of multiple rectangles.
     * 
     * @param gc
     *                the GC to use
     * @param rectangles
     *                the rectangles to draw
     * 
     * @see <a href="XDrawRectangles.html">XDrawRectangles</a>
     * @see <a href="XFillRectangles.html">XFillRectangles</a>
     * @see Request.Aggregate aggregation
     */
    public void polyRectangle(GC gc, Rectangle[] rectangles) {

        // FIXME: Handle aggregation.

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(67, 0, 3 + 2 * rectangles.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (Rectangle rec : rectangles) {
                o.writeInt16(rec.getX());
                o.writeInt16(rec.getY());
                o.writeInt16(rec.getWidth());
                o.writeInt16(rec.getHeight());
            }
            o.send();
        }
    }

    // opcode 68 - poly arc
    public void polyArc(GC gc, Arc[] arcs) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(68, 0, 3 + 3 * arcs.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (Arc arc : arcs) {
                o.writeInt16(arc.getX());
                o.writeInt16(arc.getY());
                o.writeInt16(arc.getWidth());
                o.writeInt16(arc.getHeight());
                o.writeInt16(arc.getAngle1());
                o.writeInt16(arc.getAngle2());
            }
        }
    }

    // opcode 71 - poly fill arc
    public void polyFillArc(GC gc, Arc[] arcs) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(71, 0, 3 + 3 * arcs.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            for (Arc arc : arcs) {
                o.writeInt16(arc.getX());
                o.writeInt16(arc.getY());
                o.writeInt16(arc.getWidth());
                o.writeInt16(arc.getHeight());
                o.writeInt16(arc.getAngle1());
                o.writeInt16(arc.getAngle2());
            }
        }
    }

 

    // opcode 69 - fill poly
    /**
     * This request will be aggregated.
     * 
     * @param shape
     *                valid: {@link #NOT_LAST}, {@link #BUTT},
     *                {@link #CAP_ROUND}
     * 
     * @param coordinate_mode
     *                valid: {@link #ORIGIN}, {@link #PREVIOUS}
     * 
     * @see <a href="XFillPolygon.html">XFillPolygon</a>
     * @see Request.Aggregate aggregation
     */
    public void fillPoly(GC gc, Point[] points, Fill shape, CoordinateMode coordinateMode) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(69, 0, 4 + points.length);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt8(shape.getCode());
            o.writeInt8(coordinateMode.getCode());
            o.skip(2);
            for (Point p : points) {
                o.writeInt16(p.getX());
                o.writeInt16(p.getY());
            }
            o.send();
        }
    }

    public void fillPoly(GC gc, int[] xpoints, int[] ypoints, int npoints,
                          Fill shape, CoordinateMode coordinateMode) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(69, 0, 4 + npoints);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt8(shape.getCode());
            o.writeInt8(coordinateMode.getCode());
            o.skip(2);
            for (int i = 0; i < npoints; i++) {
                o.writeInt16(xpoints[i]);
                o.writeInt16(ypoints[i]);
            }
        }
    }

    // opcode 72 - put image
    public void putSmallImage(GC gc, Image image, int y1, int y2, int x, int y) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            int offset = image.getLineByteCount() * y1;
            int length = image.getLineByteCount() * (y2 - y1);
            int p = RequestOutputStream.pad(length);

            Image.Format format = image.get_format();
            o.beginRequest(72, format.getID(), 6 + (length + p) / 4);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(image.get_width());
            o.writeInt16(y2 - y1);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt8(image.getLeftPad());
            o.writeInt8(image.get_pixmap_format().getDepth());
            o.skip(2);
            o.write(image.get_data(), offset, length);
            o.send();
        }
    }

    /**
     * Returns the contents of the specified rectangle of the drawable in the
     * format you specify.
     * 
     * opcode 73.
     * 
     * @throws EscherUnsupportedScreenBitDepthException
     */
    public Image image(int x, int y, int width, int height, int planeMask,
                       Image.Format format)
            throws EscherUnsupportedScreenBitDepthException {

        RequestOutputStream o = display.getResponseOutputStream();
        Image image;

        synchronized (o) {

            o.beginX11CoreRequest(X11CoreCommand.GetImage, format.getID());
            o.writeInt32(id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.writeInt32(planeMask);

            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                int depth = i.readInt8();
                i.skip(2);
                int len = i.readInt32() * 4;
                int visualID = i.readInt32();
                i.skip(20);
                byte[] data = new byte[len];
                i.readData(data);

                if (format == Image.Format.ZPIXMAP) {
                    // should never be null
                    VisualInfo xVisual = this.display.getVisualInfo(visualID);                   
                    if (xVisual == null) {
                        // try with the default one
                        xVisual = display.getDefaultVisual();
                    }
                    image = new ZPixmap(display, width, height,
                                        display.getDefaultPixmapFormat(), data,
                                        xVisual);
                } else {                  
                    // "Now XYPixmaps are something that I never use, and
                    // I don't think anyone else uses either, they appear to be
                    // an insane Pixmap where the data is divided into three
                    // parts, first all the red pixel values, then all the green
                    // pixels, and then all the blue pixels. I'm sure with some
                    // thought I could make up a reasonable sounding excuse for
                    // this, though it escapes me right now."
                    // From Caolan McNamara
                    // http://www.linux.ie/old-list/23540.html
                    throw new UnsupportedOperationException("Support for "
                            + "XYPixmap not yet implemented");
                }
            }
        }
        return image;
    }

    // opcode 74 - poly text8
    /**
     * @see <a href="XDrawText.html">XDrawText</a>
     */
    public void polyText(GC gc, int x, int y, Text[] texts) {

        int n = length(texts, 8);
        int p = RequestOutputStream.pad(n);
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(74, 0, 4 + (n + p) / 4);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);

            for (int i = 0; i < texts.length; i++) {
                if (texts[i].getFont() != null) {
                    o.writeInt8(255);// font-shift indicator
                    o.writeInt32(texts[i].getFont().getID()); // java = MSB
                }
                o.writeInt8(texts[i].getStr().length());
                o.writeInt8(texts[i].getDelta());
                o.writeString8(texts[i].getStr());
            }
            // Can't simply skip the padding bytes, otherwise the X server
            // would think that there are more items if the next byte in the
            // buffer is != 0, this would produce random errors.
            for (int i = 0; i < p; i++) {
                o.writeInt8(0);
            }
            o.send();
        }
    }

    // opcode 75 - poly text16
    /**
     * @see <a href="XDrawText16.html">XDrawText16</a>
     */
    public void polyText16(GC gc, int x, int y, Text[] texts) {

        int n = length(texts, 16);
        int p = RequestOutputStream.pad(n);

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(75, 0, 4 + (n + p) / 4);

            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);

            for (int i = 0; i < texts.length; i++) {
                if (texts[i].getFont() != null) {
                    o.writeInt8(255);// font-shift indicator
                    o.writeInt32(texts[i].getFont().getID()); // java = MSB
                }

                String s = texts[i].getStr();

                if (s.charAt(0) > 128) { // non-ascii
                    o.writeInt8(s.length() / 2);
                    o.writeInt8(texts[i].getDelta());
                    o.writeString8(s);
                } else {// ascii
                    o.writeInt8(s.length());
                    o.writeInt8(texts[i].getDelta());
                    o.writeString16(s);
                }
            }

            o.send();

        }
    }

    // opcode 76 - image text8
    /**
     * @see <a href="XDrawImageString.html">XDrawImageString</a>
     */
    public void imageText(GC gc, int x, int y, String s) {

        int n = s.length();
        int p = RequestOutputStream.pad(n);
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(76, n, 4 + (n + p) / 4);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeString8(s);
            o.send();
        }
    }

    // opcode 77 - image text16
    /**
     * @see <a href="XDrawImageString16.html">XDrawImageString16</a>
     */
    public void imageText16(GC gc, int x, int y, String s) {

        int n = s.length();
        int p = RequestOutputStream.pad(2 * n);

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(77, n, 4 + (2 * n + p) / 4);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeString16(s);
            o.send();
        }
    }

    // opcode 97 - query best size
    /**
     * @param klass
     *                valid: {@link #CURSOR}, {@link #TILE}, {@link #STIPPLE}
     * 
     * @see <a href="XQueryBestSize.html">XQueryBestSize</a>
     */
    public Size bestSize(Shape klass, int width, int height) {

        RequestOutputStream o = display.getResponseOutputStream();
        int w, h;
        synchronized (o) {
            o.beginRequest(97, klass.getCode(), 3);
            o.writeInt32(id);
            o.writeInt16(width);
            o.writeInt16(height);
            o.send();

            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                w = i.readInt16();
                h = i.readInt16();
                i.skip(20);
            }
        }
        return new Size(w, h);
    }

    /**
     * Draws the outline of a single arc.
     * 
     * @param gc
     *                the GC to use
     * @param x
     *                the bounding rectangle, x coordinate
     * @param y
     *                the bounding rectangle, y coordinate
     * @param w
     *                the bounding rectangle, width
     * @param h
     *                the bounding rectangle, height
     * @param angle1
     *                the start angle, from 3 o'clock ccw, in degrees
     * @param angle2
     *                the span angle, from angle1 ccw, in degrees
     * 
     * @see #polyArc(GC, Arc[])
     */
    public void arc(GC gc, int x, int y, int width, int height, int angle1,
                    int angle2) {

        // FIXME: Handle aggregation.

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(68, 0, 6);
            o.writeInt32(id);
            o.writeInt32(gc.id);

            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.writeInt16(angle1);
            o.writeInt16(angle2);
            o.send();
        }
    }

    /**
     * Fills a single arc.
     * 
     * @param gc
     *                the GC to use
     * @param x
     *                the bounding rectangle, x coordinate
     * @param y
     *                the bounding rectangle, y coordinate
     * @param w
     *                the bounding rectangle, width
     * @param h
     *                the bounding rectangle, height
     * @param angle1
     *                the start angle, from 3 o'clock ccw, in degrees
     * @param angle2
     *                the span angle, from angle1 ccw, in degrees
     * 
     * @see #polyArc(GC, Arc[])
     */
    public void fillArc(GC gc, int x, int y, int width, int height,
                         int angle1, int angle2) {

        // FIXME: Handle aggregation.

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(71, 0, 6);
            o.writeInt32(id);
            o.writeInt32(gc.id);

            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.writeInt16(angle1);
            o.writeInt16(angle2);
            o.send();
        }
    }

    /**
     * Draws a single line.
     * 
     * @param gc
     *                the GC to use
     * @param x1
     *                the start point, x coordinate
     * @param y1
     *                the start point, y coordinate
     * @param x2
     *                the end point, x coordinate
     * @param y2
     *                the end point, y coordinate
     */
    public void line(GC gc, int x1, int y1, int x2, int y2) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(65, CoordinateMode.ORIGIN.getCode(), 5);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x1);
            o.writeInt16(y1);
            o.writeInt16(x2);
            o.writeInt16(y2);
            o.send();
        }
    }

    public void segment(GC gc, int x1, int y1, int x2, int y2) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            if (o.currentOpcode() == 66 && o.fits(8)) {
                o.increaseLength(2);
                o.writeInt16(x1);
                o.writeInt16(y1);
                o.writeInt16(x2);
                o.writeInt16(y2);
            } else {
                o.beginRequest(66, CoordinateMode.ORIGIN.getCode(), 5);
                o.writeInt32(id);
                o.writeInt32(gc.id);
                o.writeInt16(x1);
                o.writeInt16(y1);
                o.writeInt16(x2);
                o.writeInt16(y2);
            }
        }
    }

    /**
     * Draws a single point.
     * 
     * @param x
     *                the x coordinate
     * @param y
     *                the y coordinate
     */
    public void point(GC gc, int x, int y) {

        // FIXME: Handle aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(64, CoordinateMode.ORIGIN.getCode(), 4);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.send();
        }
    }

    public void put_image(GC gc, Image image, int x, int y) {

        // TODO: Make use of big requests when possible.
        int max_data_byte = display.getMaximumRequestLength() - 24;
        int lbc = image.getLineByteCount();
        int request_height = lbc > 0 ? max_data_byte
                / image.getLineByteCount() : 1;
        int rem = image.get_height() % request_height;
        int request_count = image.get_height() / request_height
                + (rem == 0 ? 0 : 1);

        for (int i = 0; i < request_count; i++) {
            putSmallImage(gc, image, i * request_height, Math.min(image
                    .get_height(), (i + 1) * request_height), x, y + i
                    * request_height);
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
     * @see #polyRectangle(GC, Rectangle[])
     */
    public void rectangle(GC gc, int x, int y, int width, int height) {

        // FIXME: Handle aggregation.

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(67, 0, 5);
            o.writeInt32(id);
            o.writeInt32(gc.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.send();
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
    public void fill_rectangle(GC gc, int x, int y, int width, int height) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            if (o.currentOpcode() == 70 && o.getInt32(4) == id
                    && o.getInt32(8) == gc.id) {
                o.increaseLength(2);
                o.writeInt16(x);
                o.writeInt16(y);
                o.writeInt16(width);
                o.writeInt16(height);
            } else {
                o.beginRequest(70, 0, 5);
                o.writeInt32(id);
                o.writeInt32(gc.id);
                o.writeInt16(x);
                o.writeInt16(y);
                o.writeInt16(width);
                o.writeInt16(height);
                o.send();
            }
        }
    }

    /**
     * @see #polyText(GC, int, int, Text[])
     */
    public void text8(GC gc, int x, int y, String s, int delta, Font font) {

        polyText(gc, x, y, new Text[] {
            new Text(s, delta, font)
        });
    }

    /**
     * @see #text(GC, int, int, String, int, Font)
     */
    public void text(GC gc, int x, int y, String s) {

        polyText(gc, x, y, new Text[] {
            new Text(s, 0, null)
        });
    }

    public void text16(GC gc, int x, int y, String s) {

        polyText16(gc, x, y, new Text[] {
            new Text(s, 0, null)
        });
    }

    private int length(Text[] texts, int bit) {

        int n = 0;
        for (int i = 0; i < texts.length; i++)
            n += texts[i].length(bit);
        return n;
    }

    /**
     * @deprecated
     */
    public void rectangle(GC xgc, int x, int y, int w, int h, boolean fill) {

        if (fill)
            fill_rectangle(xgc, x, y, w, h);
        else
            rectangle(xgc, x, y, w, h);
    }
}
