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
    public void set_clip_mask (Pixmap p) { set (6, p.id); }


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
    public void set_dither (Atom a) { set (11, a.id); }


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
    
    super (render.display);
    this.render = render;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 4, 5 + attr.count ());
      o.write_int32 (id);
      o.write_int32 (drawable.id);
      o.write_int32 (format.id ());
      o.write_int32 (attr.bitmask);
      attr.write (o);
      o.send ();
    }
  }


  // render opcode 5 - change picture
  /**
   * @see <a href="XRenderChangePicture.html">XRenderChangePicture</a>
   */
  public void change (Attributes attr) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 5, 5+attr.count ());
      o.write_int32 (id);
      o.write_int32 (attr.bitmask);
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

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 7, 3 + 2 * rectangles.length);
      o.write_int32 (id);

      for (int i = 0; i < rectangles.length; i++) {
        o.write_int16 (rectangles [i].x);
        o.write_int16 (rectangles [i].y);
        o.write_int16 (rectangles [i].width);
        o.write_int16 (rectangles [i].height);
      }
      o.send ();
    }
  }


  // render opcode 7 - free picture
  /**
   * @see <a href="XRenderFreePicture.html">XRenderFreePicture</a>
   */
  public void free () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 7, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // render opcode 8 - scale
  public void scale (int color_scale, int alpha_scale, 
    DrawablePicture src, int src_x, int src_y, 
    int dst_x, int dst_y, int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 9, 8);
      o.write_int32 (src.id);
      o.write_int32 (id);
      o.write_int32 (color_scale);
      o.write_int32 (alpha_scale);
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int16 (dst_x);
      o.write_int16 (dst_y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }


  // render opcode 26 - fill rectangles
  /**
   * @see <a href="XRenderFillRectangle.html">XRenderFillRectangle</a>
   */
  public void fill_rectangle (int op, Color color, int x, int y, 
    int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 26, 7);
      o.write_int8 (op);
      o.skip (3);
      o.write_int32 (id);
      o.write_int16 (x);
      o.write_int16 (y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.write_int16 (color.red);
      o.write_int16 (color.green);
      o.write_int16 (color.blue);
      o.write_int16 (color.alpha);
      o.send ();
    }
  }
}
