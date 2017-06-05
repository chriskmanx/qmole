package gnu.x11.extension.render;

import gnu.x11.Atom;
import gnu.x11.Data;
import gnu.x11.Drawable;
import gnu.x11.GC;
import gnu.x11.Pixmap;
import gnu.x11.Rectangle;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/** Picture in RENDER. */
public class DrawablePicture extends Picture {


  public Render render;
  public Drawable drawable;


  /** ValueList for {@link DrawablePicture}. */
  public static class Attributes extends gnu.x11.ValueList {
    public final static Attributes EMPTY = new Attributes ();
    public Attributes () { super (13); }
    public void set_alpha_x_origin (int i) { set (2, i); }
    public void set_alpha_y_origin (int i) { set (3, i); }
    public void set_clip_x_origin (int i) { set (4, i); }
    public void set_clip_y_origin (int i) { set (5, i); }


    /** 
     * @param b default: false
     */
    public void set_repeat (boolean b) { set (0, b); }

    
    /** 
     * @param p possible:
     * {@link DrawablePicture#NONE} (default)
     */
    public void set_alpha_map (DrawablePicture p) { set (1, p.id); }


    /** 
     * @param p possible:
     * {@link Pixmap#NONE} (default)
     */
    public void set_clip_mask (Pixmap p) { set (6, p.getID()); }


    /** 
     * @param b default: true
     */
    public void set_graphics_exposures (boolean b) { set (7, b); }


    /** 
     * @param i valid:
     * {@link gnu.x11.GC.Values#CLIP_BY_CHILDREN} (default),
     * {@link gnu.x11.GC.Values#INCLUDE_INTERIORS}
     */
    public void set_subwindow_mode (int i) { set (8, i); }


    public static final int SHARP = 0;
    public static final int SMOOTH = 1;


    /** 
     * @param i valid:
     * {@link #SHARP},
     * {@link #SMOOTH} (default)
     */
    public void set_poly_edge (int i) { set (9, i); }

    
    public static final int PRECISE = 0;
    public static final int IMPRECISE = 1;

    
    /** 
     * @param i valid:
     * {@link #PRECISE} (default),
     * {@link #IMPRECISE}
     */
    public void set_poly_mode (int i) { set (10, i); }


    /** 
     * @param a possible:
     * {@link Atom#NONE} (default)
     */
    public void set_dither (Atom a) { set (11, a.getID()); }


    /** 
     * @param b default: false
     */
    public void set_component_alpha (boolean i) { set (12, i); }
  }
  

  // render opcode 4 - create picture
  /**
   * @see <a href="XRenderCreatePicture.html">XRenderCreatePicture</a>
   * @see Render#create_picture(Drawable, DrawablePicture.Format, 
   * DrawablePicture.Attributes)
   */
  public DrawablePicture (Render render, Drawable drawable, PictFormat format, 
                  Attributes attr) {
    
    super (render.getDisplay());
    this.render = render;

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 4, 5 + attr.count ());
      o.writeInt32 (id);
      o.writeInt32 (drawable.getID());
      o.writeInt32 (format.id ());
      o.writeInt32 (attr.getBitmask());
      attr.write (o);
      o.send ();
    }
  }


  // render opcode 5 - change picture
  /**
   * @see <a href="XRenderChangePicture.html">XRenderChangePicture</a>
   */
  public void change (Attributes attr) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 5, 5+attr.count ());
      o.writeInt32 (id);
      o.writeInt32 (attr.getBitmask());
      attr.write (o);
      o.send ();    
    }
  }


  // render opcode 6 - set picture clip rectangles
  /**
   * @see <a href="XRenderSetPictureClipRectangles.html">
   * XRenderSetPictureClipRectangles</a>
   */
  public void set_clip_rectangles (int x_origin, int y_origin,
    Rectangle [] rectangles) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 7, 3 + 2 * rectangles.length);
      o.writeInt32 (id);

      for (int i = 0; i < rectangles.length; i++) {
        o.writeInt16 (rectangles [i].getX());
        o.writeInt16 (rectangles [i].getY());
        o.writeInt16 (rectangles [i].getWidth());
        o.writeInt16 (rectangles [i].getHeight());
      }
      o.send ();
    }
  }


  // render opcode 7 - free picture
  /**
   * @see <a href="XRenderFreePicture.html">XRenderFreePicture</a>
   */
  public void free () {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 7, 2);
      o.writeInt32 (id);
      o.send ();
    }
  }


  // render opcode 8 - scale
  public void scale (int color_scale, int alpha_scale, 
    DrawablePicture src, int src_x, int src_y, 
    int dst_x, int dst_y, int width, int height) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 9, 8);
      o.writeInt32 (src.id);
      o.writeInt32 (id);
      o.writeInt32 (color_scale);
      o.writeInt32 (alpha_scale);
      o.writeInt16 (src_x);
      o.writeInt16 (src_y);
      o.writeInt16 (dst_x);
      o.writeInt16 (dst_y);
      o.writeInt16 (width);
      o.writeInt16 (height);
      o.send ();
    }
  }


  // render opcode 26 - fill rectangles
  /**
   * @see <a href="XRenderFillRectangle.html">XRenderFillRectangle</a>
   */
  public void fill_rectangle (int op, Color color, int x, int y, 
    int width, int height) {

    RequestOutputStream o = display.getResponseOutputStream();
    synchronized (o) {
      o.beginRequest (render.getMajorOpcode(), 26, 7);
      o.writeInt8 (op);
      o.skip (3);
      o.writeInt32 (id);
      o.writeInt16 (x);
      o.writeInt16 (y);
      o.writeInt16 (width);
      o.writeInt16 (height);
      o.writeInt16 (color.red);
      o.writeInt16 (color.green);
      o.writeInt16 (color.blue);
      o.writeInt16 (color.alpha);
      o.send ();
    }
  }
}
